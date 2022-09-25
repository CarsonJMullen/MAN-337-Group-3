library(tidyverse)

#Better than Home than Away

WCD <- WorldCupData %>%
  filter(!is.na(HomeGoals)) %>%
  filter(Date < '2022-09-19') %>%
  mutate(HomeScoreDifferential = HomeGoals - AwayGoals) %>%
  mutate(AwayScoreDifferential = AwayGoals - HomeGoals)

WCD %>%
  group_by(Away) %>%
  summarise(mean = mean(HomeScoreDifferential),
            count = n()) %>%
  filter(count > 10) %>%
  arrange(-mean)

WCD %>%
  group_by(Home) %>%
  summarise(mean = mean(HomeScoreDifferential),
            count = n()) %>%
  filter(count > 10) %>%
  arrange(-mean)
