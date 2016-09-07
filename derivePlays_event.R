

# prepare data -------------------------------------------------------

prep_data_from_file <- function(game_file, events) {
     
     read_csv(game_file) %>% 
          
          .[,2:ncol(.)] %>% 
          
          mutate(shot_clock = ifelse(is.na(shot_clock),0,shot_clock)) %>% 
          arrange(quarter, desc(game_clock), shot_clock, x_loc)  %>%
          
          # label ball-side
          mutate(ballside = ifelse(player_id == -1 & x_loc < 47, 'L', NA ),
                 ballside = ifelse(player_id == -1 & x_loc >= 47, 'R', ballside) ) %>% 
          
          # label court-side
          mutate(courtside = ifelse(x_loc < 47, 'L', 'R')) %>%
          
          # is player / ball in the paint
          mutate(inPaint = in_paint(x_loc, y_loc) ) %>% 
          
          # take only distinct quarter-game_clock-player combinations
          distinct(player_id, quarter, game_clock) %>% 
     
          # filter to events of interest
          filter(event.id %in% events)
     
}

prep_data_from_frame <- function(frame, game, events) {
     
     frame %>% 
          
          # filter to game of interest
          filter(gameid == game) %>% 
          
          mutate(shot_clock = ifelse(is.na(shot_clock),0,shot_clock)) %>% 
          arrange(quarter, desc(game_clock), shot_clock, x_loc)  %>%
          
          # label ball-side
          mutate(ballside = ifelse(player_id == -1 & x_loc < 47, 'L', NA ),
                 ballside = ifelse(player_id == -1 & x_loc >= 47, 'R', ballside) ) %>% 
          
          # label court-side
          mutate(courtside = ifelse(x_loc < 47, 'L', 'R')) %>%
          
          # is player / ball in the paint
          mutate(inPaint = in_paint(x_loc, y_loc) ) %>% 
          
          # take only distinct quarter-game_clock-player combinations
          distinct(player_id, quarter, game_clock) %>% 
          
          # filter to events of interest
          filter(event.id %in% events)
     
}

# compute some features on the ball ----------------------------------

get_ball_features <- function(data) {
     
     data %>% 
     filter(player_id == -1) %>% 
          mutate(dt = lag(game_clock) - game_clock) %>% 
          group_by(event.id) %>% 
          summarize(distanceTraveled = dist_traveled(x_loc, y_loc), 
                    xDistanceTraveled = ax_dist(x_loc),
                    yDistanceTraveled = ax_dist(y_loc),
                    xSpread = ax_spread(x_loc),
                    ySpread = ax_spread(y_loc),
                    elapsedTime = max(game_clock) - min(game_clock),
                    timeInPaint = sum(ifelse(inPaint == T, dt, 0)) ) 
     
} 
     

