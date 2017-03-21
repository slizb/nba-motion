
library(tidyr)
library(stringr)

set_token('8e64318f4efd95943afa5fa76b794904')

league <- "nba"
sport <- "basketball"
ep <- "players"
q_body <- list()
players <- ss_get_result(sport = sport, league = league, ep = ep,
                         query = q_body, version = 1, walk = TRUE)
players_df <- do.call("rbind", lapply(players, function(x) x$players))

players_df <- players_df %>% 
     mutate(full_name = str_c(first_name, last_name, sep = ' '),
            full_name = tolower(full_name))

write.csv(players_df, '/Volumes/nba/players/playerStats.csv')

