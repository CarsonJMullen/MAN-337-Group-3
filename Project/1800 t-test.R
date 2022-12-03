NFL_rbs_top16 <- NFL_rbs %>% slice_max(rushing_yards, n = 16)

NFL_rbs_top16$carriesb1800 <- 0
NFL_rbs_top16$yardsb1800 <- 0

for (i in 1:nrow(NFL_rbs_top50)) {
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