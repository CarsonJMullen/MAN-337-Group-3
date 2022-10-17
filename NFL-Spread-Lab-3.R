library(tidyverse)

# Have betting lines improved over time?

# It seems like betting lines have not improved over the last twenty years,
# as the median difference in the spread and actual score has remained between
# 7 and 9 points, and the percentage the favorite winning remained around 67%. 
# I would have hypothesized that with the addition of tracking data
# and the advancements in advanced analytics, the accuracy of spreads would have
# improved, but it hasn't.

# Creates new variables to measure our comparisons
NFLGameData = NFLGameData %>%
  mutate(spread_vs_real = result - spread_line) %>%
  mutate(favored_correct = ifelse(result * spread_line < 0, 0, 1)) %>%
  filter(!is.na(result))

#Group by year
NFL_By_Year <- NFLGameData %>%
  group_by(season) %>%
  summarize(median_diff = median(abs(spread_vs_real)),
            correct_percent = median(favored_correct))

# Used median rather than mean because the absolute value of the difference in
# actual vs spread is heavily skewed to the right.
ggplot(NFL_By_Year) +
  geom_line(aes(x=season, y=median_diff)) +
  ylim(0, 12)

ggplot(NFL_By_Year) +
  geom_line(aes(x=season, y=correct_percent)) +
  ylim(0, 1)

# This shows the percentage of games in a season that the home team covered the spread.
# The percentage floats around 50% which makes sense because the spread is set to 
# make gambling competitive and there should be bets on both sides of the spread.
# This shows while odds makers may not be getting more accurate in terms of
# difference to the spread or the correct team winning, it seems like the spread
# is an accurate in splitting the middle. That being said, it does not appear that
# the line has gotten tighter around 50%.

NFLGameData <- NFLGameData %>% mutate(spread_correct = spread_line > result)

NFLGameDataYearly <- NFLGameData %>% 
  group_by(season) %>%
  summarize(percent_correct = mean(spread_correct))

ggplot(NFLGameDataYearly) +
  geom_line(aes(x=season, y=percent_correct)) +
  ylim(0, 1)

