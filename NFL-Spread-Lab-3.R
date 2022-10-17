library(tidyverse)

#Have betting lines improved over time?

NFLGameData = NFLGameData %>%
  mutate(spread_vs_real = result - spread_line) %>%
  filter(!is.na(result))

NFL_By_Year <- NFLGameData %>%
  group_by(season) %>%
  summarize(avg_diff = mean(abs(spread_vs_real)))

ggplot(NFL_By_Year) +
  geom_line(aes(x=season, y=avg_diff))
