library(tidyverse)
library(lubridate)

#Cleaning Data

WCD <- WorldCupData %>%
  filter(!is.na(HomeGoals)) %>%
  filter(Date < '2022-09-19') %>%
  mutate(HomeScoreDifferential = HomeGoals - AwayGoals) %>%
  mutate(AwayScoreDifferential = AwayGoals - HomeGoals)

WCD$Home = sub("\\s+[^ ]+$", "", WCD$Home)
WCD$Away = sub(".*?? ", "", WCD$Away)

#Better Home than Away?

WCD %>%
  group_by(Away) %>%
  summarise(mean = mean(AwayScoreDifferential),
            count = n()) %>%
  filter(count > 10) %>%
  arrange(-mean)

WCD %>%
  group_by(Home) %>%
  summarise(mean = mean(HomeScoreDifferential),
            count = n()) %>%
  filter(count > 10) %>%
  arrange(-mean)

#Which country scored the second most goals in 2018?

#Answer: France and Croatia, tied at second with 14 goals
#        First place is Belgium

WCD2018Home <- WCD %>%
  filter(year(Date) == '2018') %>%
  group_by(Home) %>%
  summarize(Goals = sum(HomeGoals))
  
WCD2018Away <- WCD %>%
  filter(year(Date) == '2018') %>%
  group_by(Away) %>%
  summarize(Goals = sum(AwayGoals))

WCD2018Home$AwayGoals <- WCD2018Away$Goals
WCD2018 <- WCD2018Home %>%
  mutate(TotalGoals = Goals + AwayGoals)

#Make an argument for which team is the most exciting to watch in the past 10 years

#The three attributes of a soccer game I find the most entertaining are success, close games, and 
#high-scoring. I mutated the dataframes to create variables evaluating these metrics. 
#To address high scoring, I averaged the total number of goals scored in each games that a team played.
#To address success, I averaged the score difference for each game played
#To address close games, I calculated the percentage of games within one goal

#There are several teams that fit these characteristics, however, in the past 10 years 100% of Scotland's
#games have finished within one goal. Scotland also has a very high average of 4 goals scored a game
#in total and a slightly positive average score difference of 0.67 goals, meaning that on average
#Scotland wins games by 0.67 goals. This score differential is perfect because it is slightly positive,
#where they will win most of the time but still keep games close.

WCDExcite <- WCD %>%
  filter(year(Date) > 2012) %>%
  mutate(TotalGoals = HomeGoals + AwayGoals,
         CloseGames = ifelse(abs(HomeGoals - AwayGoals) <= 1, 1, 0))

WCDExciteHome <- WCDExcite %>%
  group_by(Home) %>%
  summarize(totalGoals = sum(TotalGoals),
            scoreDiff = sum(HomeScoreDifferential),
            numCloseGames = sum(CloseGames),
            games = n()) %>%
  arrange(Home)

WCDExciteAway <- WCDExcite%>%
  group_by(Away) %>%
  summarize(totalGoals = sum(TotalGoals),
            scoreDiff = sum(HomeScoreDifferential),
            numCloseGames = sum(CloseGames),
            games = n()) %>%
  arrange(Away)

colnames(WCDExciteHome) <- c("Team", "totalGoals", "scoreDiff", "numCloseGames", "games")
colnames(WCDExciteAway) <- c("Team", "totalGoals", "scoreDiff", "numCloseGames", "games")

WCDExciteTotal <- data.frame(Team = WCDExciteHome$Team,
                             totalGoals = WCDExciteHome$totalGoals + WCDExciteAway$totalGoals,
                             scoreDiff = WCDExciteHome$scoreDiff + WCDExciteAway$scoreDiff,
                             closeGames = WCDExciteHome$numCloseGames + WCDExciteAway$numCloseGames,
                             games = WCDExciteHome$games + WCDExciteAway$games) %>%  
  mutate(goalsPerGame = totalGoals/games,
         avgScoreDiff = scoreDiff/games,
         closeGamePercent = closeGames/games)
