library(tidyverse)
library(lubridate)

#Cleaning Data

#Filtering out NA data and adding a score differential variable
WCD <- WorldCupData %>%
  filter(!is.na(HomeGoals)) %>%
  filter(Date < '2022-09-19') %>%
  mutate(HomeScoreDifferential = HomeGoals - AwayGoals) %>%
  mutate(AwayScoreDifferential = AwayGoals - HomeGoals)

#Standardizing naming conventions to just have the country.
WCD$Home = sub("\\s+[^ ]+$", "", WCD$Home)
WCD$Away = sub(".*?? ", "", WCD$Away)

#Better Home than Away?

#Cuba has the best home field advantage in observed events
#because on average the Cuba will beat their opponent by 8.5 more goals 
#at home rather than away. This is also an extremely small sample size. Hungary has 
#the best home field advantage with over 10 games both home and away. 

#Some additional factors would be the location of the of the event because
#most of the stadiums were in neutral sites. Additionally, number of fans 
#present and supporting each team could help describe perceived home-field advantages.

#Have to create individual data frames because you group by home/away to evaluate the score of a single team
WCDBetterAway <- WCD %>%
  group_by(Away) %>%
  summarise(mean = mean(AwayScoreDifferential),
            count = n()) %>%
  arrange(-mean)

WCDBetterHome <- WCD %>%
  group_by(Home) %>%
  summarise(mean = mean(HomeScoreDifferential),
            count = n()) %>%
  arrange(-mean)

#Renaming columns to match so that when we merge them it knows to match by team
colnames(WCDBetterHome) <- c("Team", "AvgScoreDiff", "Games")
colnames(WCDBetterAway) <- c("Team", "AvgScoreDiff", "Games")

#Merging dfs. x is home and y is away
WCDBetterTotal <- merge(WCDBetterHome, WCDBetterAway, by = "Team", all = TRUE) %>%
  mutate(HomeAwayDiff = AvgScoreDiff.x - AvgScoreDiff.y)

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

WCDExcite <- WCD %>%
  filter(year(Date) > 2012) %>%
  mutate(TotalGoals = HomeGoals + AwayGoals, #total goals scored in a game
         CloseGames = ifelse(abs(HomeGoals - AwayGoals) <= 1, 1, 0)) #dummy variable which is 1 if one goal game, 0 if more than that

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
