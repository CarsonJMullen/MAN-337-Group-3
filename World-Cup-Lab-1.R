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

WCD2018Home$Goals + WCD2018Away$Goals

#Make an argument for which team is the most exciting to watch in the past 10 years

#High Scoring
WCDExciteHome <- WCD %>%
  filter(year(Date) > 2012) %>%
  mutate(TotalGoals = HomeGoals + AwayGoals)



#Close Games
