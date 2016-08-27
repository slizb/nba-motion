

library(data.table)
library(foreach)
library(doMC)
registerDoMC(cores = 4)



# find 3-pointer events for each game --------------------------------

threes <- read.csv('~/Desktop/gameinfo.csv')

games <- unique(threes$gameid)

# loop through each game, grab those events --------------------------
     # parallelize across cores
system.time(
feats <- foreach(game = games, .combine = rbind) %dopar% {
     
     events <- threes %>% 
          filter(gameid == game) %>% 
          .$event.id %>% 
          unique()
     
     # where exactly do i filter down to these events?
     
     pbp_path <- paste0('/volumes/nba/pbp/00', game, '_pbp.txt')
     track_path <- paste0('/Volumes/nba/games/00', game, '.csv.gz')
     
     df <- prep_data(track_path)
     
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
                                'event.id' = 'event.id'))
          
     }
     
     else NULL
     
})

# should switch to fread

