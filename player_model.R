
library(plyr)
library(dplyr)
library(doMC)
library(caret)
library(viridis)
registerDoMC(cores = 4)
set.seed(321)

rm(list = ls())

df <- read.csv('/Users/home/Downloads/Player_statswithfeatures.csv')

players <- unique(df$PLAYER1_ID)


preds_df <- foreach(player = players, .combine = rbind) %dopar% {
     
     train <- df %>% filter(PLAYER1_ID != player) %>% 
          select(-c(PLAYER1_ID, count, team_id, TEAM, name.player) )
     test <- df %>%  filter(PLAYER1_ID == player)
     
     seeds <- list()
     for(i in 1:6) seeds[[i]] <- sample.int(n=1000, 1)
     
     fitControl <- trainControl(method = "none",
                                seeds = seeds)
     
     gbmGrid <-  expand.grid(interaction.depth = 5,
                             n.trees = 100,
                             shrinkage = 0.1,
                             n.minobsinnode = 20)
     
     gbmFit1 <- train(X3PA ~ ., 
                      data = train,
                      method = "gbm",
                      trControl = fitControl,
                      metric = "Rsquared",
                      tuneGrid = gbmGrid,
                      verbose = FALSE,
                      tuneLength = 10)
     
     data.frame(prediction = predict(gbmFit1, test),
                actual = test$X3PA, 
                player = test$name.player,
                threePct = test$X3P.)

}

preds_df <- preds_df %>% 
     mutate(deviation = actual - prediction,
            "3P%" = threePct)

#write.csv(preds_df, '3PA_model_deviations.csv')


# more threes than expected ------------------------------------------

negdev <- preds_df %>% 
     arrange(desc(deviation) ) %>% 
     head(10) 

negdev_order <- negdev$player
negdev <- negdev %>% 
     mutate(player = factor(player, levels = negdev_order, ordered = T)) 
     
ggplot(negdev, aes(x = player,
                   y = deviation,
                   fill = `3P%`)) +
     geom_bar(stat = 'identity') +
     scale_y_continuous(breaks = 0:5, limits = c(0, 5.5)) +
     coord_flip() +
     labs(x = '',
          y = 'Deviation From Expected 3PA',
          title = 'More Threes Than Expected') + 
     scale_fill_viridis(limits = c(21.4, 45.1)) +
     theme_bw() +
     theme(text = element_text(size = 20) )
     
# fewer threes than expected -----------------------------------------

posdev <- preds_df %>% 
     arrange(deviation ) %>% 
     head(10) 

posdev_order <- posdev$player
posdev <- posdev %>% 
     mutate(player = factor(player, levels = posdev_order, ordered = T)) 

ggplot(posdev, aes(x = player,
                   y = deviation,
                   fill = `3P%`)) +
     geom_bar(stat = 'identity') +
     scale_y_continuous(breaks = -5:0, limits = c(-5.5, 0)) +
     coord_flip() +
     labs(x = '',
          y = 'Deviation From Expected 3PA',
          title = 'Fewer Threes Than Expected') + 
     scale_fill_viridis(limits = c(21.4, 45.1)) +
     theme_bw() +
     theme(text = element_text(size = 20) )














