



pbp_path <- '/volumes/nba/pbp/0021500021_pbp.txt'
playerStats_path <- '/volumes/nba/players/playerStats.csv'


get_dist_features <- function(data, 
                              pbp_path, 
                              playerStats_path = '/volumes/nba/players/playerStats.csv') {
     
     players <- get_player_stats(pbp_path, 
                                 playerStats_path)
     
     pbp <- read.csv(pbp_path)
     
     ball_matchup <- NULL
     team_feats <- NULL
     
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
          
          # find  moment of shot -----------------------------------------------
          
          event_df <- data %>% filter(event.id == event)
          
          ball <- event_df %>% 
               filter(player_id == -1)
          
          shot <- ball %>% .$radius > 11 
          
          shot_moment <- which(shot)[1]
          
          clock_at_shot <- ball %>% 
               .$game_clock %>% 
               .[shot_moment]
          
          # clip event to moment of shot ---------------------------------------
          
          if (!is.na(shot_moment)){
               
               event_df <- event_df %>% 
                    filter(game_clock < clock_at_shot)
               
          }
          
          # derive player-ball distances ---------------------------------------
          
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
                         distToDefender = closestPlayerDist[which(fense == 'd')], 
                         defenderID = closestPlayer[which(fense == 'd')]) %>%
               mutate(event.id = event) %>% 
               bind_rows(ball_matchup)
          
          team_feats <- data.frame(event.id = event, 
                                   stagnation = stagnation(event_df, o_team)) %>% 
               bind_rows(team_feats)
          
     }
     
     # derive sub-event id --------------------------------------------
     # this represents changes in on-ball defenders over the course of an event
     
     dist_features <- ball_matchup %>% 
          mutate(defenderBreak = lag(defenderID) != defenderID,
                 defenderBreak = ifelse(is.na(defenderBreak), 1, defenderBreak),
                 subEventID = cumsum(defenderBreak) ) %>% 
          group_by(event.id, subEventID) %>% 
          summarize(medDist = median(distToDefender),
                    maxDist = max(distToDefender),
                    distAtShot = distToDefender[which(row_number()==n()) ],
                    onBallHeightMismatch = onBallHeightMismatch[which(row_number()==n())],
                    onBallWeightMismatch = onBallWeightMismatch[which(row_number()==n())],
                    onBallexperienceMismatch = onBallexperienceMismatch[which(row_number()==n())],
                    onBallpositionMatchup = onBallpositionMatchup[which(row_number()==n())]) %>% 
          
          # compute features for the final on ball defender of the event
          
          group_by(event.id) %>% 
          summarize(medDefenderDist = medDist[which.max(subEventID)],
                    maxDefenderDist = maxDist[which.max(subEventID)],
                    DefenderDistAtShot = distAtShot[which.max(subEventID)],
                    onBallHeightMismatch = onBallHeightMismatch[which.max(subEventID)],
                    onBallWeightMismatch = onBallWeightMismatch[which.max(subEventID)],
                    onBallExperienceMismatch = onBallexperienceMismatch[which.max(subEventID)],
                    onBallPositionMatchup = onBallpositionMatchup[which.max(subEventID)])
     
     dist_features <- dist_features %>% 
          left_join(team_feats, by = 'event.id')
     
     return(dist_features)
     
}

