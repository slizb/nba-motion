library (dplyr)
library (readr)
library(jsonlite)

setwd("~/Code/nba-motion")
load("data/preppedData.RData")
devtools::install_github("abresler/nbastatR")
library("nbastatR") 

##Get Train Data
table(train$numberPasses)
mean(train$elapsedTime)
train <- train %>% filter (elapsedTime > 5) %>% filter(numberPasses >= 1) %>% 
  filter (elapsedTime < 35) 
mean(test$elapsedTime)

##Get All player stats
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

###First aggregating team/player from by play data
train_player <- train %>% group_by(PLAYER1_ID) %>% 
      summarize(count = n(),dt = mean(distanceTraveled), dtx=mean(xDistanceTraveled),
                dty=mean(yDistanceTraveled),eltime=mean(elapsedTime),timepaint = mean(timeInPaint),
                meddefdist = mean(medDefenderDist),maxdefdist = mean(maxDefenderDist),defdist=mean(defenderDistAtShot),
                passes = mean(numberPasses),balldefenders = mean(numberBallDefenders),teammov=mean(teamMovement),
               ochull=mean(offenseChull),dchull=mean(defenseChull),diffchull=mean(chullDiff)) %>% 
      filter(count > 10)

##Look at correlations
library(corrplot)
M <- cor(train_player[,2:13])
corrplot(M, method="circle")

##Add player stats for shooting percentage
playersstats <- players %>% select(PLAYER1_ID=id.player,team_id=teamId,name.player,TEAM,`3PA`,`3P%`,PTS)
train_player <- merge(train_player,playersstats,by="PLAYER1_ID",all.x = TRUE)
train_player[,20] <- as.numeric(train_player[,20])
train_player[,21] <- as.numeric(train_player[,21])
train_player[,22] <- as.numeric(train_player[,22])
write.csv(train_player,"Player_statswithfeatures.csv",row.names = FALSE)

##Look at correlations
library(corrplot)
datacols <- c(2:17,20:22)
M <- cor(train_player[,datacols])
corrplot(M, method="circle")

##Player model
library(h2o)
h2o.init()

train_fe <- train_player[,3:5]/train_player[,6]
train_fe[,4:14] <- train_player[,6:16]

train2 <- train_fe
train2$target <- train_player$`3PA`
alltrain.h2o <- as.h2o(train2)
train.split <- h2o.splitFrame(data=alltrain.h2o, ratios=0.75)
train.h2o <- train.split[[1]]
test.h2o <- train.split[[2]] 

x = c(1:14)
y= 15

bb.gbm <- h2o.gbm(x=x,y=y,
                  training_frame = train.h2o,
                  validation_frame = test.h2o)

h2o.performance(bb.gbm)
h2o.performance(bb.gbm, valid = TRUE)
h2o.varimp(bb.gbm)
h2o.varimp_plot(bb.gbm)
# r2	0.408757

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

###Test aggregating team/player stats at TEAM level
train_team <- train %>% group_by(team_id) %>% 
  summarize(count = n(),dt = mean(distanceTraveled), dtx=mean(xDistanceTraveled),
            dty=mean(yDistanceTraveled),eltime=mean(elapsedTime),timepaint = mean(timeInPaint),
            meddefdist = mean(medDefenderDist),maxdefdist = mean(maxDefenderDist),defdist=mean(defenderDistAtShot),
            passes = mean(numberPasses),balldefenders = mean(numberBallDefenders),teammov=mean(teamMovement),
            ochull=mean(offenseChull),dchull=mean(defenseChull),diffchull=mean(chullDiff)
            ) %>% 
  filter(count > 10)


##Look at correlations
library(corrplot)
M <- cor(train_team[,2:16])
corrplot(M, method="circle")

##Add team stats
teamstats <- team %>% select(team_id=teamId,teamName,`W%`,`3PA`,`3P%`,PTS)
train_team <- merge(train_team,teamstats,by="team_id",all.x = TRUE)
train_player[,19] <- as.numeric(train_player[,19])
train_player[,20] <- as.numeric(train_player[,20])
train_player[,21] <- as.numeric(train_player[,21])

##Look at correlations
library(corrplot)
datacols <- c(2:16,19:21)
M <- cor(train_team[,datacols])
corrplot(M, method="circle")

###Difference between Player stats and the Mean by Team
player_combined <- merge(train_player,train_team,by="team_id",all.x = TRUE)
df_player <- player_combined %>% mutate(dt = dt.x-dt.y,eltime=eltime.x-eltime.y,count=count.x-count.y,
                                        maxdefdist=maxdefdist.x-maxdefdist.y, passes=passes.x-passes.y,
                                        balldef = balldefenders.x-balldefenders.y,teammov = teammov.x-teammov.y,
                                        timepaint= timepaint.x-timepaint.y) %>% 
            select (-TEAM)###NEED TO UPDATE WITH CHULL###

df_player[,23] <- as.numeric(df_player[,23])
df_player[,24] <- as.numeric(df_player[,24])
df_player[,25] <- as.numeric(df_player[,25])

df_player <- df_player[complete.cases(df_player),]
datacols <- c(3:14,18:20,23:37,39:46)
datacols <- c(23:37)
datacols <- c(39:46)
M <- cor(df_player[,datacols])
corrplot(M, method="circle")

df_player2 <- df_player %>% select (team_id,name.player,PLAYER1_ID,count.x,dt,eltime,maxdefdist,passes,balldef,teammov,timepaint,`3PA.x`)
write.csv(df_player2,"Player_diffbyteam.csv",row.names = FALSE)

##Oklohoma
df_ok <- df_player2 %>% filter (team_id == 60)

##Player model
library(h2o)
h2o.init()

alltrain.h2o <- as.h2o(df_player2)
train.split <- h2o.splitFrame(data=alltrain.h2o, ratios=0.75)
train.h2o <- train.split[[1]]
test.h2o <- train.split[[2]] 

x = c(5:11)
y= 12

bb.gbm <- h2o.gbm(x=x,y=y,
                  training_frame = train.h2o,
                  validation_frame = test.h2o)

h2o.performance(bb.gbm)
h2o.performance(bb.gbm, valid = TRUE)
h2o.varimp(bb.gbm)
h2o.varimp_plot(bb.gbm)
## r2	0.271340

#TSNE plot
library(Rtsne)
iris_unique <- unique(df_player2[,1:10]) # Remove duplicates
iris_matrix <- as.matrix(iris_unique[1:181,5:10])
set.seed(42) # Set a seed if you want reproducible results
tsne_out <- Rtsne(iris_matrix) # Run TSNE
# Show the objects in the 2D tsne representat
plot(tsne_out$Y,col=df_player2$team_id)

###Test plots
21500206 play 1
21500307 play 30 -- not in dataset
21500309 play 27
21500286 play 20
temp <- train %>% filter (gameid==21500286,playid==20)



#TSNE plot
library(Rtsne)
datacols <- c(2:16,19:21)

iris_unique <- unique(train_team[,2:16]) # Remove duplicates
iris_matrix <- as.matrix(scale(train_team[,2:16]))

set.seed(42) # Set a seed if you want reproducible results
tsne_out <- Rtsne(iris_matrix,perplexity = 5) # Run TSNE
# Show the objects in the 2D tsne representat
plot(tsne_out$Y,col=train_team$team_id)
df = as.data.frame(tsne_out$Y)
df$repository_language = train_team$teamName
df$perc = ((train_team$`3P%`) - min(train_team$`3P%`))
df$perc = train_team$`3PA`- min(train_team$`3PA`)
df$perc = train_team$`W%`/10
ggplot(df, aes(x = V1, y = V2, label = repository_language)) + 
  geom_point(alpha = 0.8, size = df$perc, color = "tomato") +
  geom_text(alpha = .7, size = 2, hjust = -.2) +
 ggtitle("Percent")
ggsave("Perc.png")


Logos
library(grImport)
library(grConvert)
R> convertPicture("nzflag-original.svg", "nzflag-cairo.svg")
BOS <- readPicture("logos/BOS_logo.svg")
