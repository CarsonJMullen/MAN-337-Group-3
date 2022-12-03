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
  mutate(game_number = row_number(), cum_carries = cumsum(carries), cum_rushing_yards = cumsum(rushing_yards), ypc = rushing_yards/carries)

NFL_rushing_stats <- NFL_rushing_stats[,-11:-26]
NFL_rushing_stats <- NFL_rushing_stats[NFL_rushing_stats$rookie_season > 1999,]

NFL_rushing_season_stats <- NFL_rushing_stats %>%
  group_by(player_id, season) %>%
  summarise(season_num =  mean(years),
            gp = n(), 
            carries = sum(carries),
            rushing_yards = sum(rushing_yards), 
            rushing_tds = sum(rushing_tds))

cum_yards_season <- ave(NFL_rushing_season_stats$rushing_yards, NFL_rushing_season_stats$player_id, FUN = cumsum)
cum_carries <- ave(NFL_rushing_season_stats$carries, NFL_rushing_season_stats$player_id, FUN = cumsum)

NFL_rushing_season_stats$cum_rushing_yards <- cum_yards_season
NFL_rushing_season_stats$cum_carries <- cum_carries

NFL_rushing_season_stats$ypc <- NFL_rushing_season_stats$rushing_yards/NFL_rushing_season_stats$carries

NFL_rbs <- merge(NFL_rbs, 
                 NFL_rushing_stats %>% group_by(player_id) %>%
                   summarise(seasons = max(years), carries = sum(carries), rushing_yards = sum(rushing_yards)))

top_2000 <- NFL_rbs %>%
  filter(carries >= 2000)

top_2000$player_id

average_player <- NFL_rushing_season_stats %>%
  filter(player_id %in% top_2000$player_id) %>%
  group_by(season_num) %>%
  summarize(cum_carries = median(cum_carries),
            rushing_yards = median(rushing_yards))

carries_vs_season <- ggplot(average_player, aes(x=season_num, y=cum_carries)) +
  geom_line()
rush_yds_vs_season <- ggplot(average_player, aes(x=season_num, y=rushing_yards)) +
  geom_line()

carriesColor <- "#69b3a2"
rushingYdsColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(average_player, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_line(aes(y = rushing_yards), color = rushingYdsColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*0.4, name = "Rushing Yards")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = rushingYdsColor)
  )
