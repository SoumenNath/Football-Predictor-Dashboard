# app.R

library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)

# Load all datasets from 2018 onwards
season_files <- list.files("data", pattern = "epl_\\d{4}_\\d{4}_cleaned.csv", full.names = TRUE)
season_data <- lapply(season_files, read_csv)
names(season_data) <- gsub("data/epl_|_cleaned.csv", "", basename(season_files))
all_seasons <- sort(names(season_data), decreasing = TRUE)

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel("Premier League Dashboard (2018/19 - Present)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_season", "Select a Season", choices = all_seasons, selected = all_seasons[1]),
      selectInput("selected_team", "Select a Team", choices = NULL),
      checkboxInput("home_only", "Show Home Matches Only", FALSE),
      hr(),
      helpText("Explore match results, goal trends, and discipline stats.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Match Results", dataTableOutput("match_table")),
        tabPanel("Team Performance", tableOutput("performance_table")),
        tabPanel("Goal Trends", plotlyOutput("goal_plot")),
        tabPanel("Discipline", plotlyOutput("card_plot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Update team list based on selected season
  observeEvent(input$selected_season, {
    season_df <- season_data[[input$selected_season]]
    teams <- sort(unique(c(season_df$HomeTeam, season_df$AwayTeam)))
    updateSelectInput(session, "selected_team", choices = c("All Teams", teams), selected = "All Teams")
  })
  
  # Reactive: get data for selected season
  season_matches <- reactive({
    req(input$selected_season)
    season_data[[input$selected_season]]
  })
  
  # Reactive: filter matches based on selected team and checkbox
  filtered_matches <- reactive({
    data <- season_matches()
    
    if (input$selected_team == "All Teams") {
      return(data %>% arrange(Date))
    }
    
    data <- data %>%
      filter(HomeTeam == input$selected_team | AwayTeam == input$selected_team)
    
    if (input$home_only) {
      data <- data %>% filter(HomeTeam == input$selected_team)
    }
    
    data %>%
      arrange(Date) %>%
      mutate(
        Opponent = ifelse(HomeTeam == input$selected_team, AwayTeam, HomeTeam),
        Venue = ifelse(HomeTeam == input$selected_team, "Home", "Away"),
        GoalsFor = ifelse(HomeTeam == input$selected_team, FTHG, FTAG),
        GoalsAgainst = ifelse(HomeTeam == input$selected_team, FTAG, FTHG),
        Result = case_when(
          GoalsFor > GoalsAgainst ~ "Win",
          GoalsFor < GoalsAgainst ~ "Loss",
          TRUE ~ "Draw"
        )
      ) %>%
      select(Date, HomeTeam, AwayTeam, Opponent, Venue, GoalsFor, GoalsAgainst, Result,
             total_goals, goal_diff, HY, AY, HR, AR)
  })
  
  # Match Results table
  output$match_table <- renderDataTable({
    datatable(
      filtered_matches(),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # Team Performance table
  output$performance_table <- renderTable({
    data <- filtered_matches()
    
    if (input$selected_team == "All Teams") {
      season_matches() %>%
        mutate(
          Team = HomeTeam,
          GoalsFor = FTHG,
          GoalsAgainst = FTAG,
          Result = case_when(
            FTHG > FTAG ~ "Win",
            FTHG < FTAG ~ "Loss",
            TRUE ~ "Draw"
          )
        ) %>%
        group_by(Team) %>%
        summarise(
          Matches = n(),
          Wins = sum(Result == "Win"),
          Draws = sum(Result == "Draw"),
          Losses = sum(Result == "Loss"),
          GF = sum(GoalsFor),
          GA = sum(GoalsAgainst),
          GD = GF - GA,
          Avg_GF = round(mean(GoalsFor), 2),
          Avg_GA = round(mean(GoalsAgainst), 2),
          .groups = "drop"
        ) %>%
        arrange(desc(Wins))
    } else {
      tibble(
        `Matches Played` = nrow(data),
        Wins = sum(data$Result == "Win"),
        Draws = sum(data$Result == "Draw"),
        Losses = sum(data$Result == "Loss"),
        `Goals Scored` = sum(data$GoalsFor),
        `Goals Conceded` = sum(data$GoalsAgainst),
        `Avg Goals Scored` = round(mean(data$GoalsFor), 2),
        `Avg Goals Conceded` = round(mean(data$GoalsAgainst), 2),
        `Goal Difference` = sum(data$goal_diff)
      )
    }
  })
  
  # Goal Trends plot
  output$goal_plot <- renderPlotly({
    if (input$selected_team == "All Teams") {
      team_data <- season_matches() %>%
        mutate(Matchday = as.integer(factor(Date))) %>%
        group_by(HomeTeam, Matchday) %>%
        summarise(Goals = mean(FTHG), .groups = "drop") %>%
        rename(Team = HomeTeam)
      
      p <- ggplot(team_data, aes(x = Matchday, y = Goals, color = Team)) +
        geom_line() +
        labs(title = paste("Goals Scored per Matchday (", input$selected_season, ")"),
             x = "Matchday", y = "Avg Goals (Home)") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      data <- filtered_matches() %>%
        arrange(Date) %>%
        mutate(
          Matchday = row_number(),
          `Goals Scored` = GoalsFor,
          `Goals Conceded` = GoalsAgainst
        ) %>%
        pivot_longer(cols = c(`Goals Scored`, `Goals Conceded`),
                     names_to = "Metric", values_to = "Goals")
      
      p <- ggplot(data, aes(x = Matchday, y = Goals, color = Metric)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = paste("Goals Scored vs Conceded -", input$selected_team),
             x = "Matchday", y = "Goals") +
        scale_color_manual(values = c("Goals Scored" = "#0072B2", "Goals Conceded" = "#D55E00")) +
        theme_minimal(base_size = 14) +
        theme(legend.title = element_blank())
      
      ggplotly(p)
    }
  })
  
  # Discipline plot
  output$card_plot <- renderPlotly({
    data <- filtered_matches()
    
    if (input$selected_team == "All Teams") {
      card_data <- season_matches() %>%
        group_by(HomeTeam) %>%
        summarise(
          Yellow = sum(HY, na.rm = TRUE),
          Red = sum(HR, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(Team = HomeTeam) %>%
        pivot_longer(cols = c(Yellow, Red), names_to = "Card", values_to = "Count")
      
      p <- ggplot(card_data, aes(x = reorder(Team, -Count), y = Count, fill = Card)) +
        geom_col(position = "dodge") +
        labs(title = paste("Discipline by Team (", input$selected_season, ")"), x = "Team", y = "Total Cards") +
        scale_fill_manual(values = c("Yellow" = "#FFD700", "Red" = "#D62828")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    } else {
      total_yellow <- sum(ifelse(data$Venue == "Home", data$HY, data$AY), na.rm = TRUE)
      total_red    <- sum(ifelse(data$Venue == "Home", data$HR, data$AR), na.rm = TRUE)
      
      discipline_df <- tibble(
        CardType = c("Yellow Cards", "Red Cards"),
        Count = c(total_yellow, total_red)
      )
      
      p <- ggplot(discipline_df, aes(x = CardType, y = Count, fill = CardType)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = c("Yellow Cards" = "#FFD700", "Red Cards" = "#D62828")) +
        labs(title = paste("Discipline -", input$selected_team), x = NULL, y = "Total Cards") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")
      
      ggplotly(p)
    }
  })
}

# Run the app
shinyApp(ui, server)
