#Train Models
#####Linear Model####
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

#####Random Forest Model#####
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


#####Linear Model####
library(nbastatR)
library(dplyr)
library(fuzzyjoin)
library(zoo)
#Define Model
game_logs <- game_logs(seasons = c(2022, 2023, 2024), 
                       result_types = 'team', 
                       season_types = "Regular Season")

nba_games<-game_logs(seasons = c(2022, 2023, 2024), result_types = "team",
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



