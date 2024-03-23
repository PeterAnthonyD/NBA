#####Gather Testing Data Data######
######Games Schedules for Today and Odds#####
library(rvest)

url <- "https://www.basketball-reference.com/leagues/NBA_2024_games-march.html"
webpage <- read_html(url)
nba_games_df <- webpage %>% html_table(header = TRUE, fill = TRUE) %>% .[[1]]
nba_games_df$Date <- as.Date(nba_games_df$Date, format = "%a, %b %e, %Y")
nba_gamees_today = subset(nba_games_df, nba_games_df$Date==Sys.Date())
nba_gamees_today <- nba_gamees_today[, colnames(nba_gamees_today) != ""]

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

# Perform fuzzy matching left join
nba_gamees_today_home_join <- stringdist_join(nba_gamees_today_home, nba_teams_lookup,
                                              by = 'nameTeam', mode = 'left', method = "jw",
                                              max_dist = 99, distance_col = 'dist') %>%
  group_by('nameTeam') %>%
  slice_min(order_by = dist, n = 1)
nba_gamees_today_home_join <- nba_gamees_today_home_join %>% select(`Visitor/Neutral`,  slugTeam)
nba_gamees_today_home_join <- nba_gamees_today_home_join %>% rename(nameTeam = `Visitor/Neutral`, 
                                                                    HomeAbbriv = "slugTeam")
nba_gamees_today_home_join <- stringdist_join(nba_gamees_today_home_join, nba_teams_lookup, by = 'nameTeam', mode = 'left', method = "jw", max_dist = 99, distance_col = 'dist') %>%
  group_by('nameTeam') %>%
  slice_min(order_by = dist, n = 1)
nba_gamees_today_home_join <- nba_gamees_today_home_join %>% select(HomeAbbriv,
                                                                    slugTeam, last_five, last_five_against,
                                                                    last_ten, 
                                                                    last_ten_against)
nba_gamees_today_home_join <- nba_gamees_today_home_join %>% rename(slugOpponent = slugTeam)
nba_gamees_today_home_join  <- nba_gamees_today_home_join %>% rename(slugTeam = HomeAbbriv)
nba_gamees_today_home_join$locationGame = "H"

#####Make Linear predictions#####
firstscore_pred = predict.lm(nba_quickModle, nba_gamees_today_home_join,
                                                   interval = c("prediction"),
                                                   level = .95)
nba_gamees_today_home_join$firstscore = firstscore_pred[,"fit"]
nba_gamees_today_home_join$firstup = firstscore_pred[,"upr"]
nba_gamees_today_home_join$firstlow = firstscore_pred[,"lwr"]


nba_gamees_today_home_join_second <- nba_gamees_today_home_join %>% rename(slugTeam_2 = slugOpponent)
nba_gamees_today_home_join_second <- nba_gamees_today_home_join_second %>% rename(slugOpponent_2 = slugTeam)
nba_gamees_today_home_join_second <-  nba_gamees_today_home_join_second %>% rename(slugTeam = slugTeam_2,
                                                                                    slugOpponent = slugOpponent_2)

nba_gamees_today_home_join_second$locationGame = "A"
SecondScore_pred = predict.lm(nba_quickModle, nba_gamees_today_home_join_second,
                              interval = c("prediction"),
                              level = .95)

nba_gamees_today_home_join_second$secondscore <-SecondScore_pred[,"fit"]
nba_gamees_today_home_join_second$secondup <-SecondScore_pred[,"upr"]
nba_gamees_today_home_join_second$secondlwr <-SecondScore_pred[,"lwr"]

predictions <- nba_gamees_today_home_join_second %>%
  select(slugOpponent, slugTeam, firstscore, firstup, firstlow, secondscore, secondup, secondlwr)

predictions$total_pred = predictions$firstscore+
  predictions$secondscore

predictions$total_pred_lwr = predictions$firstlow+
  predictions$secondlwr

predictions$total_pred_up = predictions$firstup+
  predictions$secondup

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

odds_join <- merge(predictions, odds_total_nba_join, 
                   by.x = c("slugOpponent", "slugTeam"),
                   by.y = c("slugTeam.x", "slugTeam.y"), all.x = T)

odds_join_export <- odds_join %>% select(slugOpponent, home_team,slugTeam, away_team, firstscore, 
                                         secondscore, total_pred, outcomes_price, outcomes_point) %>%
  rename(HomeAbbrv = slugOpponent,
         AwayAbbrv = slugTeam,
         HomeScore = firstscore,
         AwayScore = secondscore,
         over_odds = outcomes_price,
         line = outcomes_price)

odds_join_export$Line_Pred_diff = abs(odds_join_export$total_pred - odds_join_export$outcomes_point)
odds_join_export$Bet_Over_Under = ifelse(odds_join_export$total_pred > odds_join_export$outcomes_point, "Over", "Under")
odds_join_export<-na.omit(odds_join_export)
write.csv(odds_join_export, file = paste(Sys.Date(), "predictions.csv", sep = "_"))

#####Make Random Forrest predictions#####

nba_gamees_today_rf <- nba_gamees_today_home_join %>% select(slugTeam,slugOpponent,
                                                             last_five,
                                                             last_five_against
                                                             #lastgame_over_lastfive,
                                                             #nba_games$cumulative_sum +
                                                             
                                                             #locationGame,
                                                             #last_ten,
                                                             #last_ten_against
)
nba_gamees_today_rf$`"nameTeam"`<-NULL
nba_gamees_today_rf$dateGame = today()

nba_games_rf$ptsTeam <- NULL

nba_gamees_today_rf <- rbind.data.frame(nba_games_rf, nba_gamees_today_rf)

firstscore_pred = predict(rf, nba_gamees_today_rf)

nba_gamees_today_rf_second <- nba_gamees_today_rf %>% rename(slugTeam_2 = slugOpponent)
nba_gamees_today_rf_second <- nba_gamees_today_rf_second %>% rename(slugOpponent_2 = slugTeam)
nba_gamees_today_rf_second <-  nba_gamees_today_rf_second %>% rename(slugTeam = slugTeam_2,
                                                                     slugOpponent = slugOpponent_2)

nba_gamees_today_rf_second$locationGame = "A"

SecondScore_pred = predict(rf, nba_gamees_today_rf_second,
                           interval = c("prediction"),
                           level = .95)

nba_gamees_today_rf_second$firstscore_rf <- firstscore_pred
nba_gamees_today_rf_second$secondscore_rf <- SecondScore_pred

nba_gamees_today_rf_second<-subset(nba_gamees_today_rf_second, nba_gamees_today_rf_second$dateGame==today())
predictions <- nba_gamees_today_rf_second %>%
  select(slugOpponent, slugTeam, firstscore_rf, secondscore_rf)

predictions$total_pred_rf = predictions$firstscore_rf+
  predictions$secondscore_rf

predictions$slugOpponent <- as.character(predictions$slugOpponent)
predictions$slugTeam <- as.character(predictions$slugTeam)

odds_join_rf <- merge(predictions, odds_total_nba_join, 
                   by.x = c("slugOpponent", "slugTeam"),
                   by.y = c("slugTeam.x", "slugTeam.y"), all.x = T)

odds_join_export_2 <- odds_join_rf %>% select(slugOpponent, home_team,slugTeam, away_team, firstscore_rf, 
                                           secondscore_rf, total_pred_rf, outcomes_price, outcomes_point) %>%
  rename(HomeAbbrv = slugOpponent,
         AwayAbbrv = slugTeam,
         HomeScore_rf = firstscore_rf,
         AwayScore_rf = secondscore_rf,
         over_odds = outcomes_price,
         line = outcomes_price,
         home_team_rf = home_team,
         away_team_rf = away_team,
         outcomes_point_rf = outcomes_point)

odds_join_export_2$Line_Pred_diff_rf = abs(odds_join_export_2$total_pred - odds_join_export_2$outcomes_point)
odds_join_export_2$Bet_Over_Under_rf = ifelse(odds_join_export_2$total_pred > odds_join_export_2$outcomes_point, "Over", "Under")
odds_join_export_2<-na.omit(odds_join_export_2)
write.csv(odds_join_export_2, file = paste(Sys.Date(), "predictions_rf.csv", sep = "_"))



          
#####Make Multiple Year Linear Predictions#####