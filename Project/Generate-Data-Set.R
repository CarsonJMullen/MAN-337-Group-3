library(nflfastR)
library(dplyr)
library(ggplot2)

NFL_stats <- nflfastR::load_player_stats(seasons = TRUE)

NFL_rushing_stats <- NFL_stats %>% filter(position_group == "RB")

NFL_rbs <- NFL_rushing_stats %>% group_by(player_id) %>% 
  summarise(rookie_season = min(season), last_season = max(season))

NFL_rushing_stats<- merge(NFL_rushing_stats, NFL_rbs)

NFL_rushing_stats$years <- NFL_rushing_stats$season - NFL_rushing_stats$rookie_season + 1  

NFL_rushing_stats <- NFL_rushing_stats %>%
  arrange(player_id, season, week) %>%
  group_by(player_id) %>%
  mutate(game_number = row_number(), cum_carries = cumsum(carries), cum_rushing_yards = cumsum(rushing_yards))

NFL_rushing_stats <- NFL_rushing_stats[,-11:-26]
NFL_rushing_stats <- NFL_rushing_stats[NFL_rushing_stats$rookie_season > 1999,]

NFL_rushing_season_stats <- NFL_rushing_stats %>%
  group_by(player_id, season) %>%
  summarise(season_num =  mean(years),
            gp = n(), 
            carries = sum(carries), 
            rushing_yards = sum(rushing_yards), 
            rushing_tds = sum(rushing_tds))

NFL_rushing_season_stats$ypc <- NFL_rushing_season_stats$rushing_yards/NFL_rushing_season_stats$carries

NFL_rbs <- merge(NFL_rbs, 
                 NFL_rushing_stats %>% group_by(player_id) %>%
                   summarise(seasons = max(years), carries = sum(carries), rushing_yards = sum(rushing_yards)))
