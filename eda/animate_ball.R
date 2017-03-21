
ball <- toy_play %>% 
     filter(player_id == -1)

b <- ggplot(data = ball, aes(y = radius,
                             x = factor(player_id),
                             frame = 720 - game_clock)) +
     
     #geom_emoji(emoji = '1f3c0') +
     geom_point(color = 'orange',
                size = 10) +
     
     geom_text(label = 'Basket Height---',
               size = 10,
               y = 10,
               x = .8) +
     
     ylab('Ball Height') +
     
     theme_bw() +
     
     theme(text = element_text(size = 20),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title.x = element_blank() )
     
bounce <- grob(b)





gg_animate(b, outfile = "~/outfile.gif", 
           convert = "gm convert",
           interval = .02)


