
library(readr)
library(ranger)
library(Boruta)

threes <- read_csv('~/Desktop/gameinfo.csv') %>% 
     mutate(EVENTMSGTYPE = factor(EVENTMSGTYPE),
            spacing = off_area - def_area)

table(threes$EVENTMSGTYPE)

ggplot(data = threes, aes(x = EVENTMSGTYPE )) +
     geom_bar()

M <- ranger(EVENTMSGTYPE ~ off_area + def_area + dist_ball_1 + dist_ball_2 + dist_ball_1d + dist_ball_2d,
            data = threes)

B <- Boruta(formula = EVENTMSGTYPE ~ spacing + off_area + def_area + dist_ball_1 + dist_ball_2 + dist_ball_1d + dist_ball_2d,
            data = threes,
            doTrace = 2)

par(mar = c(7,4,2,2))
plot(B, las = 2, xlab = '' )
