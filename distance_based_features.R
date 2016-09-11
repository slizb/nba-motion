

get_matchup_feats <- function(play, players, play_df, o_players, d_players) {
     
     plyng_plyrs <- unique(play_df$player_id) %>% 
          .[which(. != -1)]
     
     pdm <- NULL
     
     for (p in plyng_plyrs) {
          
          col <- data.frame(distance = player_dist(play_df, -1, p),
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
     
     matchup_feats <- ball_dist %>% 
          left_join(players, by = c('closestPlayer' = 'id.x')) %>% 
          group_by(moment) %>% 
          summarize(offenseHeight = height[which(fense == 'o')],
                    defenseHeight = height[which(fense == 'd')],
                    offenseWeight = weight[which(fense == 'o')],
                    defenseWeight = weight[which(fense == 'd')],
                    onBallHeightMismatch = height[which(fense == 'o')] - height[which(fense == 'd')],
                    onBallWeightMismatch = weight[which(fense == 'o')] - weight[which(fense == 'd')],
                    onBallexperienceMismatch = years_of_experience[which(fense == 'o')] - years_of_experience[which(fense == 'd')],
                    # need to get wingspan mismatch in here
                    onBallpositionMatchup = str_c(position_abbreviation[which(fense == 'o')],
                                                  '-',
                                                  position_abbreviation[which(fense == 'd')]),
                    distToDefender = closestPlayerDist[which(fense == 'd')], 
                    defenderID = closestPlayer[which(fense == 'd')]) %>%
          mutate(playid = play) 
     
     return(matchup_feats)
     
}

get_team_feats <- function(playid, play_df, o_team) {
     
     team_feats <- data.frame(playid = playid, 
                              stagnation = stagnation(play_df, o_team) )
     
     return(team_feats)
     
}

###!!! fix parse_teams to reference play instead of event... 
### need to build helper function to grab o_team based on player1id in play_df
parse_teams <- function(pbp, players, player1ID) {
     
     # define offense / defense -------------------------------------------
     
     o_team <- pbp %>% 
          filter(PLAYER1_ID == player1ID) %>% 
          .$PLAYER1_TEAM_ID %>% 
          mode()
     
     # if something goes wrogn, skip...
     
     bad_parse <- ifelse( any(length(o_team) == 0, is.na(o_team) ), T, F)
     
     d_team <- unique(pbp$PLAYER1_TEAM_ID) %>% 
          .[. != o_team & !is.na(.)]
     
     # define players on each team ----------------------------------------
     
     if (bad_parse) {
          
          return(list('bad_parse' = bad_parse) )
     
     } else {
      
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
          
          return(list('bad_parse' = bad_parse,
                      'o_team' = o_team,
                      'd_team' = d_team,
                      'o_players' = o_players,
                      'd_players' = d_players) )
              
     }
     
}

clip_play_to_shot <- function(play_df) {
     
     ball <- play_df %>% 
          filter(player_id == -1)
     
     shot <- ball %>% .$radius > 11 
     
     shot_moment <- which(shot)[1]
     
     clock_at_shot <- ball %>% 
          .$game_clock %>% 
          .[shot_moment]
     
     # clip play to moment of shot ---------------------------------------
     
     if (!is.na(shot_moment)){
          
          play_df %>% 
               filter(game_clock < clock_at_shot)
          
     } else play_df
     
}

step_one <- function(data, 
                     pbp,
                     players,
                     playerStats_path = '/volumes/nba/players/playerStats.csv') {
     
     matchup_feats <- NULL
     team_feats <- NULL
     
     for (play in unique(data$playid)) {
          
          # filter to play ----------------------------------------------------
          
          play_df <- data %>% filter(playid == play)
          
          play_df <- clip_play_to_shot(play_df)
          
          # parse teams --------------------------------------------------------
          
          PLAYER1_ID <-  mode(play_df$PLAYER1_ID)
          
          teams <- parse_teams(pbp, players, PLAYER1_ID ) 
          
          if (teams$bad_parse) next
          
          # derive player-ball distances ---------------------------------------
          
          more_team_feats <- tryCatch(get_team_feats(play, play_df, teams$o_team),
                                      error = function(e) return(NULL) ) 
          team_feats <- bind_rows(team_feats, more_team_feats)
          
          more_matchup_feats <- tryCatch(get_matchup_feats(play,
                                                           players,
                                                           play_df, 
                                                           teams$o_players, 
                                                           teams$d_players),
                                         error = function(e) return(NULL) )
          
          matchup_feats <- bind_rows(matchup_feats, more_matchup_feats)
          
     }
   
     return(list('matchup_feats' = matchup_feats, 
                 'team_feats' = team_feats) )
   
}

aggregate_matchup_feats <- function(fine_matchup_feats) {
     
     # derive sub-event id 
     # this represents changes in on-ball defenders over the course of a play
     
     fine_matchup_feats %>% 
          mutate(previousDefender = lag(defenderID),
                 defenderBreak = previousDefender != defenderID,
                 defenderBreak = ifelse(is.na(defenderBreak), 1, defenderBreak),
                 subPlayID = cumsum(defenderBreak) ) %>% 
          group_by(playid, subPlayID) %>% 
          summarize(medDist = median(distToDefender),
                    maxDist = max(distToDefender),
                    distAtShot = distToDefender[which(row_number()==n())],
                    offenseHeight = offenseHeight[which(row_number()==n())],
                    defenseHeight = defenseHeight[which(row_number()==n())],
                    offenseWeight = offenseWeight[which(row_number()==n())],
                    defenseWeight = defenseWeight[which(row_number()==n())],
                    onBallHeightMismatch = onBallHeightMismatch[which(row_number()==n())],
                    onBallWeightMismatch = onBallWeightMismatch[which(row_number()==n())],
                    onBallexperienceMismatch = onBallexperienceMismatch[which(row_number()==n())],
                    onBallpositionMatchup = onBallpositionMatchup[which(row_number()==n())]) %>% 
          
          # compute features for the final on ball defender of the play
          
          group_by(playid) %>% 
          summarize(medDefenderDist = medDist[which.max(subPlayID)],
                    maxDefenderDist = maxDist[which.max(subPlayID)],
                    offenseHeight = offenseHeight[which.max(subPlayID)],
                    defenseHeight = defenseHeight[which.max(subPlayID)],
                    offenseWeight = offenseWeight[which.max(subPlayID)],
                    defenseWeight = defenseWeight[which.max(subPlayID)],
                    defenderDistAtShot = distAtShot[which.max(subPlayID)],
                    onBallHeightMismatch = onBallHeightMismatch[which.max(subPlayID)],
                    onBallWeightMismatch = onBallWeightMismatch[which.max(subPlayID)],
                    onBallExperienceMismatch = onBallexperienceMismatch[which.max(subPlayID)],
                    onBallPositionMatchup = onBallpositionMatchup[which.max(subPlayID)])
     
}

get_dist_features <- function(data, 
                              pbp_path, 
                              playerStats_path = '/volumes/nba/players/playerStats.csv') {
     
     players <- get_player_stats(pbp_path, 
                                 playerStats_path)
     
     pbp <- read.csv(pbp_path)
     
     processed_data <- step_one(data, pbp, players)
     
     dist_features <- aggregate_matchup_feats(processed_data$matchup_feats)
     
     dist_features <- dist_features %>% 
          left_join(processed_data$team_feats, by = 'playid')
     
     return(dist_features)
     
}

