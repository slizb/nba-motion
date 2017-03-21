

# convex hull area ---------------------------------------------------

chull_area <- function(X,Y) {
     
     #Calculates the convex hull area
     df_hull <- data.frame(X = X, Y = Y)
     c.hull <- chull(df_hull)
     #You need five points to draw four line segments, so we add the first set of points at the end
     c.hull <- c(c.hull, c.hull[1])
     chull.coords <- df_hull[c.hull ,]
     chull.poly <- Polygon(chull.coords, hole = F)
     chull.area <- chull.poly@area
     return (chull.area)
     
}

get_chull <- function(play_df, teamid) {
     
     end_of_play <- min(play_df$game_clock)
     end_minus_2 <- play_df$game_clock[which.min(abs(end_of_play + 2 - play_df$game_clock) )]
     
     clipped_teamid <- teamid %>% 
          as.character() %>% 
          str_sub(-2) %>% 
          as.integer()
     
     chull_moment <- play_df %>% 
          filter(game_clock == end_minus_2,
                 team_id == clipped_teamid) %>% 
          .[1:5,]
     
     chull_area(chull_moment$x_loc, chull_moment$y_loc)
     
}

# ball distance ------------------------------------------------------

dist_traveled <- function(xloc, yloc) {

     dx <- diff(xloc) %>% abs()
     dy <- diff(yloc) %>% abs()
     sqrt(dx^2 + dy^2) %>%  sum()

}

stagnation <- function(play_df, o_team) {
     
     elapsed_time <- play_df$game_clock %>% 
          { max(.) - min(.) }
     
     o_team_short <- substr(o_team, 9, 10) %>% 
          as.numeric()
     
     team_distance <- play_df %>% 
          filter(team_id == o_team_short) %>% 
          group_by(player_id) %>% 
          summarize(dist_traveled = dist_traveled(x_loc, y_loc)) %>% 
          .$dist_traveled %>% 
          sum()
     
     team_distance / elapsed_time
     
}

ax_dist <- function(loc) {
     
     diff(loc) %>% 
          abs() %>% 
          sum()
     
}

ax_spread <- function(loc) {
     
     max(loc) -  min(loc)
     
}

# ball in paint ------------------------------------------------------

in_paint <- function(x, y) {
     
     y_bound <- y > 19 & y < 31
     
     x_bound <- (x > 0 & x < 19) | (x > 75 & x < 94)
     
     x_bound & y_bound
     
}

# posession ----------------------------------------------------------

quarter_side <- function(my_chull, q) {
     
     # this function takes a quarter number and a dataframe with chull areas 
     # for each moment of a game by team. it returns a dataframe that labels 
     # offensive side for each team
     
     # calculate avg chull over the course of each event
     
     pos <- my_chull %>%
          group_by(event.id, team_id, quarter) %>% 
          summarize(avg_chull = mean(chull),
                    med_chull = median(chull),
                    court_side = most_frequent(court_side) ) %>% 
          
          # determine the "bigger team" during each event for the specified quarter
          # on a given side of the court this indicates possession for that side
          
          filter(quarter == q) %>%
          group_by(event.id) %>% 
          summarize(bigger_team = team_id[which.max(med_chull)],
                    court_side = court_side[which(is.na(team_id))[1]] ) %>% 
          mutate(bigger_team = factor(bigger_team))
     
     # compute the frequency of plays on each court side for the bigger team
     
     table(pos$bigger_team, pos$court_side) %>% 
          data.frame() %>% 
          rename(o_team_id = Var1) %>% 
          group_by(o_team_id) %>% 
          summarize(side = Var2[which.max(Freq)]) 
     
}

# most frequent ------------------------------------------------------

mode <- function(x) {
     
     ux <- unique(x)
     ux[which.max(tabulate(match(x, ux)))]
     
}

most_frequent <- function(x) {
     
     mf_wrapper <- function(y) {
          
          most_f <- y %>% 
               .[!is.na(.)] %>% 
               table() %>% 
               which.max() %>% 
               names()
          
          if (is.null(most_f)){
               
               most_f <- 'none'
               
          }
          
          return(most_f)
          
     }
     
     tryCatch( mf_wrapper(x),
               error = function(e) return('none'),
               warning = function(e) return('none'))
     
}

# last non na --------------------------------------------------------

last_non_na <- function(x) {
     
     last_wrapper <- function(y) {
          vec_index <- which(!is.na(y) ) %>% 
               max()
          
          last_nna <- y[vec_index]
          
          if (is.null(last_nna)){
               last_nna <- 'none'
          }
          
          return(last_nna)
     }
     
     tryCatch( last_wrapper(x),
               error = function(e) return('none'),
               warning = function(e) return('none'))
     
}

# join player stats to play by play-----------------------------------

get_player_stats <- function(pbp_file, player_file) {
     
     pbp <- read.csv(pbp_file)
     players_df <- read.csv(player_file)
     
     player_link <- bind_rows(data.frame(id = pbp$PLAYER1_ID, 
                                         name = pbp$PLAYER1_NAME, 
                                         team = pbp$PLAYER1_TEAM_ID), 
                              data.frame(id = pbp$PLAYER2_ID, 
                                         name = pbp$PLAYER2_NAME, 
                                         team = pbp$PLAYER2_TEAM_ID) ) %>% 
          distinct()
     
     
     player_link %>% 
          mutate(name = tolower(name)) %>% 
          left_join(players_df, by = c('name' = 'full_name')) 
     
}


# distance matrix ----------------------------------------------------

player_dist <- function(df, player_id_A, player_id_B) {
     #Functions finds the distance of the player, assumes you have a dataframe all.movements with player info
     df <- df[which((df$player_id == player_id_A | df$player_id == player_id_B) ), ]
     dfA <- df %>% filter (player_id == player_id_A) %>% select (x_loc,y_loc) 
     dfB <- df %>% filter (player_id == player_id_B) %>% select (x_loc,y_loc) 
     df.l <- 1:nrow(dfA)
     distsdf <- unlist(lapply(df.l,function(x) {dist(rbind(dfA[x,], dfB[x,]))}))
     return(distsdf)
}





