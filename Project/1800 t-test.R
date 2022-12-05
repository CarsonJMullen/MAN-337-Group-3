NFL_rbs_top16 <- NFL_rbs %>% slice_max(rushing_yards, n = 16)

NFL_rbs_top16$carriesb1800 <- 0
NFL_rbs_top16$yardsb1800 <- 0

for (i in 1:nrow(NFL_rbs_top16)) {
  NFL_rbs_top16[i,8] = max((NFL_rushing_stats %>% filter(player_id == NFL_rbs_top16[i,1])
                            %>% filter(cum_carries < 1800))$cum_carries)
  NFL_rbs_top16[i,9] = max((NFL_rushing_stats %>% filter(player_id == NFL_rbs_top16[i,1])
                            %>% filter(cum_carries < 1800))$cum_rushing_yards)
}

NFL_rbs_top16$ypcb1800 <- NFL_rbs_top16$yardsb1800/NFL_rbs_top16$carriesb1800
NFL_rbs_top16$carriesa1800 <- NFL_rbs_top16$carries - NFL_rbs_top16$carriesb1800
NFL_rbs_top16$yardsa1800 <- NFL_rbs_top16$rushing_yards - NFL_rbs_top16$yardsb1800
NFL_rbs_top16$ypca1800 <- NFL_rbs_top16$yardsa1800/NFL_rbs_top16$carriesa1800

t.test(NFL_rbs_top16$ypcb1800, y = NFL_rbs_top16$ypca1800, paired = TRUE)





NFL_pbp <- nflfastR::load_pbp(seasons = TRUE)
NFL_pbp <- NFL_pbp %>% filter(season > 1999) %>% 
  filter(rusher_player_id %in% NFL_rbs$player_id) %>%
  filter(play_type=="run")
NFL_rushing_pbp <- NFL_pbp %>% select(play_id, rusher_player_id, rushing_yards, epa)
NFL_rushing_pbp <- NFL_rushing_pbp %>% filter(rusher_player_id %in% NFL_rbs$player_id[NFL_rbs$carries > 2000])
NFL_rushing_pbp <- NFL_rushing_pbp %>% arrange(play_id) %>%
  group_by(rusher_player_id) %>%
  mutate(carry = row_number())
t.test(NFL_rushing_pbp$rushing_yards[NFL_rushing_pbp$carry < 1800], NFL_rushing_pbp$rushing_yards[NFL_rushing_pbp$carry >= 1800])
