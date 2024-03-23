set.seed(69)
library(randomForest)
library(datasets)
library(caret)

nba_games_rf <- nba_games %>% select(ptsTeam, slugTeam,slugOpponent,
                                       last_five,
                                       last_five_against,
                                     dateGame)
                                       #lastgame_over_lastfive,
                                       #nba_games$cumulative_sum +
                                       #locationGame,
                                       #last_ten,
                                       #last_ten_against)
nba_games_rf$nameTeam <-NULL

nba_games_rf$slugTeam<-as.factor(nba_games_rf$slugTeam)
nba_games_rf$slugOpponent<-as.factor(nba_games_rf$slugOpponent)
#nba_games_rf$locationGame<-as.factor(nba_games_rf$locationGame)

nba_games_rf<-na.omit(nba_games_rf)
rf <- randomForest(ptsTeam ~ ., data = nba_games_rf,
                   ntree = 150)

#nba_gamees_today_home_join$slugTeam<-as.factor(nba_gamees_today_home_join$slugTeam)
#levels(nba_gamees_today_home_join$slugTeam) <- levels(nba_games_rf$slugTeam)

#nba_gamees_today_home_join$slugOpponent<-as.factor(nba_gamees_today_home_join$slugOpponent)
#levels(nba_gamees_today_home_join$slugOpponent) <- levels(nba_games_rf$slugOpponent) 

#nba_gamees_today_home_join$locationGame<-as.factor(nba_gamees_today_home_join$locationGame)
#levels(nba_gamees_today_home_join$locationGame) <- levels(nba_games_rf$locationGame)


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


# nba_gamees_today_rf_second$slugTeam<-as.factor(nba_gamees_today_rf_second$slugTeam)
# levels(nba_gamees_today_rf_second$slugTeam) <- levels(nba_games_rf$slugTeam)
# 
# nba_gamees_today_rf_second$slugOpponent<-as.factor(nba_gamees_today_rf_second$slugOpponent)
# levels(nba_gamees_today_rf_second$slugOpponent) <- levels(nba_games_rf$slugOpponent) 
# 
# nba_gamees_today_rf_second$locationGame<-as.factor(nba_gamees_today_rf_second$locationGame)
# levels(nba_gamees_today_rf_second$locationGame) <- levels(nba_games_rf$locationGame)

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

odds_join <- merge(predictions, odds_total_nba_join, 
                   by.x = c("slugOpponent", "slugTeam"),
                   by.y = c("slugTeam.x", "slugTeam.y"), all.x = T)

odds_join_export_2 <- odds_join %>% select(slugOpponent, home_team,slugTeam, away_team, firstscore_rf, 
                                         secondscore_rf, total_pred_rf, outcomes_price, outcomes_point) %>%
  rename(HomeAbbrv = slugOpponent,
         AwayAbbrv = slugTeam,
         HomeScore = firstscore_rf,
         AwayScore = secondscore_rf,
         over_odds = outcomes_price,
         line = outcomes_price)

odds_join_export_2$Line_Pred_diff = abs(odds_join_export_2$total_pred - odds_join_export_2$outcomes_point)
odds_join_export_2$Bet_Over_Under = ifelse(odds_join_export_2$total_pred > odds_join_export_2$outcomes_point, "Over", "Under")
write.csv(odds_join_export_2, file = paste(Sys.Date(), "predictions_rf.csv", sep = "_"))


















