
rm(list = ls())

library(tidyr)
library(zoo)
library(sp)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(foreach)
library(doMC)
registerDoMC(cores = 4)

source('functions.R')
source('derivePlays_event.R')
source('distance_based_features.R')

CSVS <- c('/Volumes/nba/three_plays/three_plays_brad_001-200.csv',
          '/Volumes/nba/three_plays/three_plays_brad_201_300.csv',
          '/Volumes/nba/three_plays/three_plays_brad_301_400.csv',
          '/Volumes/nba/three_plays/three_plays_brad_401_631.csv')

#threes <- read_csv(CSVS[1])

bound_feats <- NULL

system.time(

for (csv in CSVS) {
     
     threes <- read_csv(csv)
     
     threes$gameid <- as.numeric(threes$gameid)
     
     games <- unique(threes$gameid)
     
     # loop through each game, grab those events --------------------------
     # parallelize across cores

          feats <- foreach(game = games, .combine = rbind) %dopar% {
               
               pbp_path <- paste0('/volumes/nba/pbp/00', game, '_pbp.txt')
               track_path <- paste0('/Volumes/nba/games/00', game, '.csv.gz')
               
               df <- prep_data_from_frame(threes, game)
               
               ball_feats <- tryCatch(get_ball_features(df) %>% 
                                           mutate(gameid = game),
                                      error = function(e) return(NULL) )
               
               dist_feats <- tryCatch(get_dist_features(df, pbp_path) %>% 
                                           mutate(gameid = game),
                                      error = function(e) return(NULL) )
               
               # join together ball and dist feats
               
               if (!is.null(ball_feats) & !is.null(dist_feats)) {
                    
                    ball_feats %>% 
                         left_join(dist_feats,
                                   by = c('gameid' = 'gameid',
                                          'playid' = 'playid'))
                    
               }
               
               else NULL
               
          }
     
     target <- threes %>% 
          group_by(gameid, playid) %>% 
          summarize(EVENTMSGTYPE = mode(EVENTMSGTYPE),
                    PLAYER1_ID = mode(PLAYER1_ID),
                    team_id = team_id[which(PLAYER1_ID == player_id)[1] ] )
     
     feats <- feats %>% 
          left_join(target, by = c('gameid' = 'gameid', 
                                   'playid' = 'playid' ))
     
     bound_feats <- bound_feats %>% 
          bind_rows(feats)
     
} )

write.csv(bound_feats, 'event_features_9-20-16.csv')


