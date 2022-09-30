library(dplyr)

# Read data
NFLGameData <- read.csv("https://raw.githubusercontent.com/UTSportsAnalytics/data/main/NFLGameData.csv")

NFLHomeQBPointsScored <- NFLGameData %>% 
  # Only use data from 2021 season
  filter(season == 2021) %>%
  # Aggregate data by the home QB
  group_by(home_qb_name) %>%
  # Get the mean home points scored by each home QB
  summarise(MeanHomePointsScored = mean(home_score)) %>%
  # Change the column name for the home QB name
  select(QB = home_qb_name, MeanHomePointsScored)

NFLAwayQBPointsScored <- NFLGameData %>% 
  # Only use data from 2021 season
  filter(season == 2021) %>%
  # Aggregate data by the home QB
  group_by(away_qb_name) %>%
  # Get the mean home points scored by each home QB
  summarise(MeanAwayPointsScored = mean(away_score)) %>%
  # Change the column name for the home QB name
  select(QB = away_qb_name, MeanAwayPointsScored)

# Merge into single data frame
NFLQBPointsScored <- merge(NFLHomeQBPointsScored, NFLAwayQBPointsScored)


NFLHomeQBOTs <- NFLGameData %>%
  # Use data from the 2015 season to the 2022 season. Games that have been
  # compleeted have NA in the overtime field so we exclude them using that field
  filter(season >= 2015 & season <= 2022 & !is.na(overtime)) %>%
  # Aggregate data by the home QB
  group_by(home_qb_name) %>%
  # Overtime is denoted with a 1 so summing it gives the number of OTs
  summarise(HomeOTs = sum(overtime)) %>%
  # Change the column name for the home QB name
  select(QB = home_qb_name, HomeOTs)

NFLAwayQBOTs <- NFLGameData %>%
  # Use data from the 2015 season to the 2022 season. Games that have been
  # compleeted have NA in the overtime field so we exclude them using that field
  filter(season >= 2015 & season <= 2022 & !is.na(overtime)) %>%
  # Aggregate data by the away QB
  group_by(away_qb_name) %>%
  # Overtime is denoted with a 1 so summing it gives the number of OTs
  summarise(AwayOTs = sum(overtime)) %>%
  # Change the column name for the away QB name
  select(QB = away_qb_name, AwayOTs)

# Merge into single data frame
NFLTotalQBOTs <- merge(NFLHomeQBOTs, NFLAwayQBOTs)

# Add a column for total OTs by adding Home and Away OTs
NFLTotalQBOTs <- NFLTotalQBOTs %>%
  mutate(TotalOTs = HomeOTs + AwayOTs)

# Since the timeline is pretty large, you'd want to know how many games the QB
# started, if they retired during the time frame or if they were drafted/became
# the starter during the time frame. The how many games started number is
# also important because playoffs are included so even if it was over one 
# season, not all QBs will play in the same number of games.
