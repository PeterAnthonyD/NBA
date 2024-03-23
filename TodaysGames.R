library(rvest)

url <- "https://www.basketball-reference.com/leagues/NBA_2024_games-march.html"
webpage <- read_html(url)
nba_games_df <- webpage %>% html_table(header = TRUE, fill = TRUE) %>% .[[1]]


nba_games_df$Date <- as.Date(nba_games_df$Date, format = "%a, %b %e, %Y")

nba_gamees_today = subset(nba_games_df, nba_games_df$Date==Sys.Date())

nba_gamees_today <- nba_gamees_today[, colnames(nba_gamees_today) != ""]


