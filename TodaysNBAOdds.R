odds_2 <- try(toa_sports_odds(sport_key = 'basketball_nba', 
                    regions = 'us', 
                    markets = 'totals', 
                    odds_format = 'decimal',
                    date_format = 'iso'))

odds_2_filter <- odds_2 %>% filter(bookmaker == "DraftKings") %>% filter(outcomes_name == "Over")

odds_total_nba <- odds_2_filter %>% select(commence_time, home_team, away_team, outcomes_price, outcomes_point, market_key)

only_abbrv <- nba_teams_lookup %>% select(slugTeam, nameTeam)

odds_total_nba_join <- merge(odds_total_nba, only_abbrv, by.x = "home_team", by.y = "nameTeam", all.x = T)

odds_total_nba_join <- merge(odds_total_nba_join, only_abbrv, by.x = "away_team", by.y = "nameTeam", all.x = T)


