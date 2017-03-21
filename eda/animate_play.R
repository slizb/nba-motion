?
library(animation)
library(ggplot2)
library(gganimate)
library(emoGG)
source("~/Documents/thesis/nba/__function_fullcourt.R")


g <- fullcourt() +
     ### overlay play
     geom_point(data = toy_play, aes(x = x_loc,
                                     y = y_loc,
                                     frame = 720 - game_clock,
                                     color = factor(team_id) ),
                size = 3) 


#play_ball <- grid.arrange(g, b, ncol = 2, nrow =1)

gg_animate(g, 
           filename = "~/play.mp4",
           saver = "mp4",
           convert = "gm convert",
           interval = .02,
           title_frame = F)




