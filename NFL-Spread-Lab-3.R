library(tidyverse)

# Have betting lines improved over time?

# It seems like betting lines have not improved over the last twenty years,
# as the median difference in the spread and actual score has remained between
# 7 and 9 points. I would have hypothesized that with the addition of tracking data
# and the advancements in advanced analytics, the accuracy of spreads would have
# improved.

NFLGameData = NFLGameData %>%
  mutate(spread_vs_real = result - spread_line) %>%
  filter(!is.na(result))

# Originally used mean, but decided to use median to account for the large 
#outliers when a big upset occurs. 

NFL_By_Year <- NFLGameData %>%
  group_by(season) %>%
  summarize(median_diff = median(abs(spread_vs_real)))

ggplot(NFL_By_Year) +
  geom_line(aes(x=season, y=median_diff)) +
  ylim(0, 10)

