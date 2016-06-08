
# calculate each team's chull at each moment of the game

my_chull <- df %>% 
     group_by(event.id, team_id, quarter, game_clock) %>% 
     summarize(chull = chull_area(x_loc, y_loc),
               court_side = most_frequent(courtside) )

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
          rename(team_id = Var1) %>% 
          group_by(team_id) %>% 
          summarize(side = Var2[which.max(Freq)]) 
          
}
     
# loop through plays, label posession
# need to flag mismatched plays (possible fast breaks / other noise)

f






