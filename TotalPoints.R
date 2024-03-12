library(nbastatR)

nba_schedule = seasons_schedule(seasons = 2024, season_types = "Regular Season")


nba_games<-game_logs(seasons = 2024, result_types = "team",
              season_types = "Regular Season", nest_data = F,
              assign_to_environment = TRUE, return_message = TRUE)

nba_games<-