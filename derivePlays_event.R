
library(ggplot2)
library(tidyr)
library(zoo)
library(readr)
library(dplyr)
library(stringr)

rm(list=ls())

source('functions.R')

# prepare data -------------------------------------------------------

my_game <- "~/Documents/thesis/nba/data/0021500021.csv.gz"

prep_data <- function(game_file) {
     
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
          distinct(player_id, quarter, game_clock)
     
}

data <- prep_data(my_game) 

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
     
ball <- get_ball_features(data)
     
# group data  --------------------------------------------------------

check <- player_dist_matrix(data, 3)

plays <- data %>% 
     group_by(event.id) %>% 
     summarize(count = n(),
               play_court_side = factor(most_frequent(courtside)),
               #mf_ball_court_side = most_frequent(ballside) ,
               last_ball_court_side = last_non_na(ballside) ,
               quarter = as.numeric(most_frequent(quarter))
               )  %>% 
     
     mutate(div11 = count/11,
            not_div_by_11 = abs(div11-round(div11) ) > 0 ) %>% 
     left_join(ball, by = 'event.id')  
     #left_join(threes, by = 'event.id')

# determine & label posession ----------------------------------------

# compute chull by team for each moment of the game

my_chull <- df %>% 
     group_by(event.id, team_id, quarter, game_clock) %>% 
     summarize(chull = chull_area(x_loc, y_loc),
               court_side = most_frequent(courtside) )

# determine offensive side for each quarter

oside_by_team <- NULL

for (q in unique(df$quarter) ) {
     
     oside_by_team <- quarter_side(my_chull, q) %>% 
          mutate(quarter = q) %>% 
          bind_rows(oside_by_team)
     
}

plays <- plays %>% 
     left_join(oside_by_team,
               by = c('quarter' = 'quarter',
                      'play_court_side' = 'side') )

# need to flag / filter mismatched plays (possible fast breaks / other noise)



     

