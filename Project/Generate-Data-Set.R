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

view <- NFL_rushing_season_stats %>%
  filter(player_id %in% top_2000$player_id)

#Key Examples: 00-0019647, 00-0022736 (Steven Jackson), 00-0026164 (Chris Johnson)

#Creating Dataset For Average Player over 2000 Carries
average_player <- NFL_rushing_season_stats %>%
  filter(player_id %in% top_2000$player_id) %>%
  group_by(season_num) %>%
  summarize(cum_carries = mean(cum_carries),
            rushing_yards = mean(rushing_yards),
            ypc = mean(ypc))

#---------Creating DF of Single Players for Their Individual Graphs -----------

#Todd Gurley
ToddGurley <- NFL_rushing_season_stats %>%
  filter(player_id == "00-0032241")

#Shaun Alexander
ShaunAlexander <- NFL_rushing_season_stats %>%
  filter(player_id == "00-0019647")

#Chris Johnson
ChrisJohnson <- NFL_rushing_season_stats %>%
  filter(player_id == "00-0026164")

#Matt Forte
MattForte <- NFL_rushing_season_stats %>%
  filter(player_id == "00-0026184")

#LeSean McCoy
LeSeanMcCoy <- NFL_rushing_season_stats %>%
  filter(player_id == "00-0027029")

#Frank Gore
FrankGore <- NFL_rushing_season_stats %>%
  filter(player_id == "00-0023500")
  
#-------------------------------------------------------------------------------
  
#Defining Colors for Graphs
carriesColor <- "#000000"
rushingYdsColor <- "#FF0000"
ypcColor <- rgb(0, 0, 1)

#Average Player Graph, Yearly Rushing Yards
ggplot(average_player, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_line(aes(y = rushing_yards/0.5), color = rushingYdsColor) +
  geom_point(aes(y = rushing_yards/0.5), color = rushingYdsColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*0.5, name = "Rushing Yards")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = rushingYdsColor)
  ) + 
  labs(title = "Average Player over 2000 Carries", x = "Season Number") +
  xlim(1, 10)

#Average Player Graph, YPC
ggplot(average_player, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  geom_line(aes(y = ypc/.00125), color = ypcColor) +
  geom_point(aes(y = ypc/.00125), color = ypcColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*.00125, name = "YPC")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = ypcColor)
  ) + 
  labs(title = "Average Player over 2000 Carries", x = "Season Number") +
  xlim(1, 10)

#Todd Gurley, Yearly Results
ggplot(ToddGurley, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_line(aes(y = rushing_yards/1), color = rushingYdsColor) +
  geom_point(aes(y = rushing_yards/1), color = rushingYdsColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*1, name = "Rushing Yards")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = rushingYdsColor)
  ) + 
  labs(title = "Todd Gurley", x = "Season Number")

#Todd Gurley, YPC
ggplot(ToddGurley, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  geom_line(aes(y = ypc/.00125), color = ypcColor) +
  geom_point(aes(y = ypc/.00125), color = ypcColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*.00125, name = "YPC")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = ypcColor)
  ) + 
  labs(title = "Todd Gurley", x = "Season Number")

#Shaun Alexander, Yearly Results
ggplot(ShaunAlexander, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_line(aes(y = rushing_yards/1), color = rushingYdsColor) +
  geom_point(aes(y = rushing_yards/1), color = rushingYdsColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*1, name = "Rushing Yards")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = rushingYdsColor)
  ) + 
  labs(title = "Shaun Alexander", x = "Season Number")

#Shaun Alexander, YPC
ggplot(ShaunAlexander, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  geom_line(aes(y = ypc/.00125), color = ypcColor) +
  geom_point(aes(y = ypc/.00125), color = ypcColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*.00125, name = "YPC")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = ypcColor)
  ) + 
  labs(title = "Shaun Alexander", x = "Season Number")

#Chris Johnson, Yearly Results
ggplot(ChrisJohnson, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_line(aes(y = rushing_yards/1), color = rushingYdsColor) +
  geom_point(aes(y = rushing_yards/1), color = rushingYdsColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*1, name = "Rushing Yards")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = rushingYdsColor)
  ) + 
  labs(title = "Chris Johnson", x = "Season Number")

#Chris Johnson, YPC
ggplot(ChrisJohnson, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  geom_line(aes(y = ypc/.00125), color = ypcColor) +
  geom_point(aes(y = ypc/.00125), color = ypcColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*.00125, name = "YPC")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = ypcColor)
  ) + 
  labs(title = "Chris Johnson", x = "Season Number")

#Matt Forte, Yearly Results
ggplot(MattForte, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_line(aes(y = rushing_yards/1), color = rushingYdsColor) +
  geom_point(aes(y = rushing_yards/1), color = rushingYdsColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*1, name = "Rushing Yards")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = rushingYdsColor)
  ) + 
  labs(title = "Matt Forte", x = "Season Number")

#Matt Forte, YPC
ggplot(MattForte, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  geom_line(aes(y = ypc/.00125), color = ypcColor) +
  geom_point(aes(y = ypc/.00125), color = ypcColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*.00125, name = "YPC")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = ypcColor)
  ) + 
  labs(title = "Matt Forte", x = "Season Number")

#LeSean McCoy, Yearly Results
ggplot(LeSeanMcCoy, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_line(aes(y = rushing_yards/1), color = rushingYdsColor) +
  geom_point(aes(y = rushing_yards/1), color = rushingYdsColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*1, name = "Rushing Yards")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = rushingYdsColor)
  ) + 
  labs(title = "LeSean McCoy", x = "Season Number")

#LeSean McCoy, YPC
ggplot(LeSeanMcCoy, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  geom_line(aes(y = ypc/.00125), color = ypcColor) +
  geom_point(aes(y = ypc/.00125), color = ypcColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*.00125, name = "YPC")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = ypcColor)
  ) + 
  labs(title = "LeSean McCoy", x = "Season Number")

#Frank Gore, Yearly Results
ggplot(FrankGore, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_line(aes(y = rushing_yards/1), color = rushingYdsColor) +
  geom_point(aes(y = rushing_yards/1), color = rushingYdsColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*1, name = "Rushing Yards")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = rushingYdsColor)
  ) + 
  labs(title = "Frank Gore", x = "Season Number")

#Frank Gore, YPC
ggplot(FrankGore, aes(x = season_num)) +
  geom_line(aes(y = cum_carries), color = carriesColor) + 
  geom_point(aes(y = cum_carries), color = carriesColor) +
  geom_line(aes(y = ypc/.00125), color = ypcColor) +
  geom_point(aes(y = ypc/.00125), color = ypcColor) +
  
  scale_y_continuous(
    name = "Cumulative Carries",
    sec.axis = sec_axis(~.*.00125, name = "YPC")
  ) + 
  
  theme(
    axis.title.y = element_text(color = carriesColor),
    axis.title.y.right = element_text(color = ypcColor)
  ) + 
  labs(title = "Frank Gore", x = "Season Number")




            
