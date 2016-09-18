library (dplyr)
library (readr)
library(jsonlite)

setwd("~/Code/nba-motion")
load("data/preppedData.RData")
devtools::install_github("abresler/nbastatR")
library("nbastatR") 

##Train EDA
table(train$numberPasses)
mean(train$elapsedTime)
train <- train %>% filter(numberPasses >= 1) %>% filter(elapsedTime > 5)
mean(test$elapsedTime)

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

###Test aggregating team/player stats
train_player <- train %>% group_by(PLAYER1_ID) %>% 
      summarize(count = n(),dt = mean(distanceTraveled), dtx=mean(xDistanceTraveled),
                                      dty=mean(yDistanceTraveled),eltime=mean(elapsedTime),timepaint = mean(timeInPaint),
                                      meddefdist = mean(medDefenderDist),maxdefdist = mean(maxDefenderDist),defdist=mean(defenderDistAtShot),
                                      passes = mean(numberPasses),balldefenders = mean(numberBallDefenders),teammov=mean(teamMovement)) %>% 
      filter(count > 10)

##Look at correlations
library(corrplot)
M <- cor(train_player[,2:13])
corrplot(M, method="circle")

##Add player stats
playersstats <- players %>% select(PLAYER1_ID=id.player,team_id=teamId,name.player,TEAM,`3PA`,`3P%`,PTS)
train_player <- merge(train_player,playersstats,by="PLAYER1_ID",all.x = TRUE)
train_player[,17] <- as.numeric(train_player[,17])
train_player[,18] <- as.numeric(train_player[,18])
train_player[,19] <- as.numeric(train_player[,19])

##Look at correlations
library(corrplot)
datacols <- c(2:13,17:19)
M <- cor(train_player[,datacols])
corrplot(M, method="circle")

##Player model
library(h2o)
h2o.init()

train_fe <- train_player[,3:7]/train_player[,6]
train_fe[,6:12] <- train_player[,7:13]

train2 <- train_fe
train2$target <- train_player$`3PA`
alltrain.h2o <- as.h2o(train2)
train.split <- h2o.splitFrame(data=alltrain.h2o, ratios=0.75)
train.h2o <- train.split[[1]]
test.h2o <- train.split[[2]] 

x = c(1:12)
y= 13

bb.gbm <- h2o.gbm(x=x,y=y,
                  training_frame = train.h2o,
                  validation_frame = test.h2o)

h2o.performance(bb.gbm)
h2o.performance(bb.gbm, valid = TRUE)
h2o.varimp(bb.gbm)
h2o.varimp_plot(bb.gbm)

bb.rf <- h2o.randomForest(x=x,y=y,
                  training_frame = train.h2o,
                  validation_frame = test.h2o)

h2o.performance(bb.rf)
h2o.performance(bb.rf, valid = TRUE)
h2o.varimp(bb.rf)
h2o.varimp_plot(bb.rf)


bb.glm <- h2o.glm(x=x,y=y,
                  training_frame = train.h2o,
                  validation_frame = test.h2o)

h2o.performance(bb.glm, valid = TRUE)
h2o.varimp(bb.glm)

###Test aggregating team/player stats
train_team <- train %>% group_by(team_id) %>% 
  summarize(count = n(),dt = mean(distanceTraveled), dtx=mean(xDistanceTraveled),
            dty=mean(yDistanceTraveled),eltime=mean(elapsedTime),timepaint = mean(timeInPaint),
            meddefdist = mean(medDefenderDist),maxdefdist = mean(maxDefenderDist),defdist=mean(defenderDistAtShot),
            passes = mean(numberPasses),balldefenders = mean(numberBallDefenders),teammov=mean(teamMovement)) %>% 
  filter(count > 10)


##Look at correlations
library(corrplot)
M <- cor(train_team[,2:13])
corrplot(M, method="circle",addCoef.col = TRUE)

##Add team stats
teamstats <- team %>% select(team_id=teamId,teamName,`W%`,`3PA`,`3P%`,PTS)
train_team <- merge(train_team,teamstats,by="team_id",all.x = TRUE)
train_player[,17] <- as.numeric(train_player[,17])
train_player[,18] <- as.numeric(train_player[,18])
train_player[,19] <- as.numeric(train_player[,19])

##Look at correlations
library(corrplot)
datacols <- c(2:13,15:18)
M <- cor(train_team[,datacols])
corrplot(M, method="circle")

player_combined <- merge(train_player,train_team,by="team_id",all.x = TRUE)
df_player <- player_combined %>% mutate(dt = dt.x-dt.y,eltime=eltime.x-eltime.y,count=count.x-count.y,
                                        maxdefdist=maxdefdist.x-maxdefdist.y)

library(corrplot)

df_player[,23] <- as.numeric(df_player[,23])
df_player[,24] <- as.numeric(df_player[,24])
df_player[,25] <- as.numeric(df_player[,25])

df_player <- df_player[complete.cases(df_player),]
datacols <- c(3:14,18:20,23:37,39:46)
datacols <- c(23:37)
datacols <- c(39:46)
M <- cor(df_player[,datacols])
corrplot(M, method="circle")
