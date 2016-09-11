
rm(list = ls())

library(tidyr)
library(zoo)
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

# find 3-pointer events for each game --------------------------------

threes <- read_csv('/Volumes/nba/three_plays_brad2.csv')

threes$gameid <- as.numeric(threes$gameid)

games <- unique(threes$gameid)

# loop through each game, grab those events --------------------------
     # parallelize across cores
system.time(
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
     
})

target <- threes %>% 
     select(gameid, playid, EVENTMSGTYPE) %>% 
     distinct(gameid, playid, EVENTMSGTYPE)

feats <- feats %>% 
     left_join(target, by = c('gameid' = 'gameid', 
                              'playid' = 'playid' ))

