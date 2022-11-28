library(nflfastR)
library(dplyr)

NFL_stats <- nflfastR::load_player_stats(seasons = TRUE)

NFL_rushing_stats <- NFL_stats %>% filter(position_group == "RB")

NFL_rbs <- NFL_rushing_stats %>% group_by(player_id) %>% 
  summarise(rookie_season = min(season), last_season = max(season))

NFL_rushing_stats<- merge(NFL_rushing_stats, NFL_rbs)

NFL_rushing_stats$years <- NFL_rushing_stats$season - NFL_rushing_stats$rookie_season + 1  

NFL_rushing_stats <- NFL_rushing_stats %>%
  arrange(player_id, season, week) %>%
  group_by(player_id) %>%
  mutate(game_number = row_number())
