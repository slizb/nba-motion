


players <- get_player_stats('~/Documents/thesis/nba/data/0021500021_pbp.txt', 
                            '/volumes/nba/players/playerStats.csv' )

pbp <- read.csv('~/Documents/thesis/nba/data/0021500021_pbp.txt')


ball_matchup <- NULL

for (event in unique(data$event.id)) {
     
     # define offense / defense -------------------------------------------
     
     o_team <- pbp %>% 
          filter(EVENTNUM == event) %>% 
          .$PLAYER1_TEAM_ID
     
     # if event is not in pbp, skip...
     
     if (length(o_team) == 0) next
     if (is.na(o_team)) next
     
     d_team <- unique(pbp$PLAYER1_TEAM_ID) %>% 
          .[. != o_team & !is.na(.)]
     
     # define players on each team ----------------------------------------
     
     o_players <- players %>%
          filter(team == o_team) %>% 
          .$id.x %>% 
          data.frame(player = .,
                     fense = 'o')
     
     d_players <- players %>%
          filter(team == d_team) %>% 
          .$id.x %>% 
          data.frame(player = .,
                     fense = 'd')
     
     # derive player-ball distances ---------------------------------------
     
     event_df <- data %>% filter(event.id == event)
     
     plyng_plyrs <- unique(event_df$player_id) %>% 
          .[which(. != -1)]
     
     pdm <- NULL
     
     for (p in plyng_plyrs) {
          
          col <- data.frame(distance = player_dist(event_df, -1, p),
                            player = p) %>% 
               mutate(moment = row_number() )
          
          pdm <- bind_rows(pdm, col )
          
     }
     
     # find players from each team closest to ball ------------------------
     
     ball_dist <- pdm  %>% 
          left_join(bind_rows(o_players, d_players),
                    by = 'player') %>% 
          group_by(moment, fense) %>% 
          summarize(closestPlayer = player[which.min(distance)][1],
                    closestPlayerDist = min(distance) )
     
     # derive mismatches --------------------------------------------------
     
     ball_matchup <- ball_dist %>% 
          left_join(players, by = c('closestPlayer' = 'id.x')) %>% 
          group_by(moment) %>% 
          summarize(onBallHeightMismatch = height[which(fense == 'o')] - height[which(fense == 'd')],
                    onBallWeightMismatch = weight[which(fense == 'o')] - weight[which(fense == 'd')],
                    onBallexperienceMismatch = years_of_experience[which(fense == 'o')] - years_of_experience[which(fense == 'd')],
                    # need to get wingspan mismatch in here
                    onBallpositionMatchup = str_c(position_abbreviation[which(fense == 'o')],
                                                  '-',
                                                  position_abbreviation[which(fense == 'd')]),
                    distToDefender = closestPlayerDist[which(fense == 'd')]) %>%
          mutate(event.id = event) %>% 
          bind_rows(ball_matchup)
     
}






