library(tidyverse)
library(lubridate)

# Load raw data
raw_data <- read_csv("data/EPL_Results_1993-2022.csv")

# Inspect column names
names(raw_data)

# Clean and filter recent season (optional)
cleaned_data <- raw_data %>%
  
  #Create new features
  mutate(
    Date = as.Date(DateTime),
    match_week = week(Date),
    goal_diff = FTHG - FTAG,
    total_goals = FTHG + FTAG
  ) %>%
  filter(Date >= as.Date("2020-08-01") & Date <= as.Date("2021-06-01")) %>%
  select(
    Season, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR,
    HS, AS, HST, AST, HC, AC,
    HF, AF, HY, AY, HR, AR,
    goal_diff, total_goals, match_week
  )

write_csv(cleaned_data, "data/epl_2021_2022_cleaned.csv")