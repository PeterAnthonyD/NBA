library(nbastatR)
library(dplyr)
library(fuzzyjoin)
library(zoo)
#Define Model
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

nba_quickModle<-lm(ptsTeam ~ as.factor(slugTeam) + 
                     as.factor(slugOpponent) +
                     last_five +
                     last_five_against,
                     #lastgame_over_lastfive +
                     #cumulative_sum +
                     #factor(locationGame),
                     #last_ten +
                     #last_ten_against,
                   data = nba_games)
summary(nba_quickModle)

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

predictions_martch_13 <- nba_gamees_today_home_join_second %>%
  select(slugOpponent, slugTeam, firstscore, firstup, firstlow, secondscore, secondup, secondlwr)

predictions_martch_13$total_pred = predictions_martch_13$firstscore+
  predictions_martch_13$secondscore

predictions_martch_13$total_pred_lwr = predictions_martch_13$firstlow+
  predictions_martch_13$secondlwr

predictions_martch_13$total_pred_up = predictions_martch_13$firstup+
  predictions_martch_13$secondup

odds_join <- merge(predictions_martch_13, odds_total_nba_join, 
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
write.csv(odds_join_export, file = paste(Sys.Date(), "predictions.csv", sep = "_"))

          