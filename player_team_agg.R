library (dplyr)
library (readr)
library(jsonlite)

setwd("~/Code/nba-motion")

devtools::install_github("abresler/nbastatR")
library("nbastatR") 


##All player stats
players_2015 <-
  get_nba_season_players(
    year.season_start = 2015,
    include_only_rostered_players = F,
    return_message = T
  )

##Grabbed stats from NBA web page, this is just the merge
players_2015_split <- read_csv("data/player_stats.csv")
players_2015_split$name.player <- players_2015_split$Player
players <- merge(players_2015_split,players_2015,by="name.player",all.x = TRUE)
players <- players %>% select (-Player,-id.season,-slug.player)    
players$teamId <- substr(players$id.team, 9, nchar(players$id.team))    
write.csv(players,"data/players.csv",row.names = FALSE)

##Grabbed stats from NBA web page, this is just the merge
team_2015 <- read_csv("data/team_stats.csv")
team_2015$teamName <- team_2015$Team
team.file <-fromJSON("data/team.json")
team.file <- team.file %>% select(teamId,abbreviation,simpleName,teamName)
team <- merge(team_2015,team.file,by="teamName",all.x = TRUE)
team <- team %>% select (-Team)
team$teamId <- substr(team$teamId, 9, nchar(team$teamId))
write.csv(team,"data/team.csv",row.names = FALSE)

