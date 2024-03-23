odds_2 <- try(toa_sports_odds(sport_key = 'basketball_nba', 
                    regions = 'us', 
                    markets = 'totals', 
                    odds_format = 'decimal',
                    date_format = 'iso'))

odds_2_filter <- odds_2 %>% filter(bookmaker == "DraftKings") %>% filter(outcomes_name == "Over")

odds_total_nba <- odds_2_filter %>% select(commence_time, home_team, away_team, outcomes_price, outcomes_point, market_key)

game_logs <- game_logs(seasons = 2024, 
                       result_types = 'team', 
                       season_types = "Regular Season")

nba_games<-game_logs(seasons = 2024, result_types = "team",
                     season_types = "Regular Season", nest_data = F,
                     assign_to_environment = TRUE, return_message = TRUE)

nba_games$pointsAgainst <- nba_games$ptsTeam - nba_games$plusminusTeam

nba_games <- nba_games %>%
  arrange(dateGame) %>%  # Replace 'date_column' with the actual date column
  group_by(nameTeam) %>%
  mutate(last_five = zoo::rollmean(ptsTeam, k = 5, fill = NA, align = "right"),
         last_ten = zoo::rollmean(ptsTeam, k = 10, fill = NA, align = "right"),
         last_five_against = zoo::rollmean(pointsAgainst, k = 5, fill = NA, align = "right"),
         last_ten_against = zoo::rollmean(ptsTeam, k = 10, fill = NA, align = "right"))
nba_teams_lookup <- nba_games %>% select(slugTeam, nameTeam, last_five, dateGame, last_five_against, 
                                         last_ten, 
                                         last_ten_against)
nba_teams_lookup <- nba_teams_lookup %>%
  group_by(nameTeam) %>%
  filter(dateGame == max(dateGame))
nba_teams_lookup <- unique(nba_teams_lookup)
nba_teams_lookup$nameTeam <- gsub("LA Clippers", "Los Angeles Clippers", nba_teams_lookup$nameTeam)
nba_gamees_today_home <-  nba_gamees_today %>% select("Home/Neutral",
                                                      "Visitor/Neutral")
nba_gamees_today_home <- nba_gamees_today_home %>% rename(nameTeam = `Home/Neutral`)
only_abbrv <- nba_teams_lookup %>% select(slugTeam, nameTeam)

odds_total_nba_join <- merge(odds_total_nba, only_abbrv, by.x = "home_team", by.y = "nameTeam", all.x = T)

odds_total_nba_join <- merge(odds_total_nba_join, only_abbrv, by.x = "away_team", by.y = "nameTeam", all.x = T)


