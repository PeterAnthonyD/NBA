#Run this script to get outcome

source("/Users/peterdola/Library/Mobile Documents/com~apple~CloudDocs/Documents/BeatVegas/NBAPredictions/TrainModels.R")
source("/Users/peterdola/Library/Mobile Documents/com~apple~CloudDocs/Documents/BeatVegas/NBAPredictions/TotalPoints.R")


####Establish Betting Rules and Notifications####

what2bet<-cbind.data.frame(odds_join_export, odds_join_export_2)

what2bet_2 <- what2bet %>% select(home_team, away_team, HomeScore, AwayScore, total_pred, Bet_Over_Under,
                                  HomeScore_rf, AwayScore_rf, total_pred_rf, outcomes_point, Bet_Over_Under_rf,
                                  outcomes_point)
what2bet_2$Home_mean = (what2bet_2$HomeScore+what2bet$HomeScore_rf)/2
what2bet_2$Away_mean = (what2bet_2$AwayScore+what2bet$AwayScore_rf)/2
what2bet_2$Total_mean = (what2bet_2$total_pred+what2bet$total_pred_rf)/2
what2bet_2$Total_diff = (what2bet_2$Total_mean-what2bet$outcomes_point)



what2bet_summary <- what2bet_2 %>%
  select(home_team, away_team, Total_mean, Bet_Over_Under, Bet_Over_Under_rf, Total_diff, outcomes_point) %>%
  filter(Bet_Over_Under == Bet_Over_Under_rf) %>%
  arrange(desc(abs(Total_diff)))
