
library(magrittr)
library(plyr)
library(dplyr)
library(doMC)
library(caret)
library(viridis)
library(caret)
library(tidyr)
registerDoMC(cores = 4)
set.seed(321)

rm(list = ls())

df <- read.csv('/Users/home/Downloads/Player_statswithfeatures.csv')

trainIndex <- createDataPartition(df$X3PA, 
                                  p = .8,
                                  list = F,
                                  times = 1)

train <- df[ trainIndex,]
test  <- df[-trainIndex,]


players <- unique(train$PLAYER1_ID)

models <- list()

preds_df <- NULL

seeds <- list()
for(i in 1:6) seeds[[i]] <- sample.int(n=1000, 1)

for (player in players) {
     #foreach(player = players, .combine = rbind) %dopar% {
     
     train_players <- train %>% filter(PLAYER1_ID != player) %>% 
          select(-c(PLAYER1_ID, count, team_id, TEAM, name.player) )
     oob_player <- train %>%  filter(PLAYER1_ID == player)
     
     fitControl <- trainControl(method = "none",
                                seeds = seeds)
     
     gbmGrid <-  expand.grid(interaction.depth = 5,
                             n.trees = 100,
                             shrinkage = 0.1,
                             n.minobsinnode = 20)
     
     gbmFit  <- train(X3PA ~ ., 
                      data = train_players,
                      method = "gbm",
                      trControl = fitControl,
                      metric = "Rsquared",
                      tuneGrid = gbmGrid,
                      verbose = FALSE)
     
     actual <- test$X3PA
     preds <- predict(gbmFit, test)
     performance <- postResample(preds, actual)
     
     p_df <- data.frame(prediction = predict(gbmFit, oob_player),
                        actual = oob_player$X3PA, 
                        player = oob_player$name.player,
                        threePct = oob_player$X3P.,
                        RMSE = performance[1],
                        RSquared = performance[2])
     
     preds_df %<>% bind_rows(p_df) 
     
     models[[oob_player$name.player]] <- gbmFit
     
}
     

# apply median player model to hold-out ------------------------------

med_player <- preds_df$player[which.min(abs(preds_df$RMSE - median(preds_df$RMSE) ))]

holdout_preds <- data.frame(prediction = predict(models[[med_player]], test),
                            actual = test$X3PA, 
                            player = test$name.player,
                            threePct = test$X3P.,
                            RMSE = NA,
                            RSquared = NA)

# compute deviation, propensity  for each player ---------------------

preds_df %<>% 
     bind_rows(holdout_preds) %>% 
     mutate(deviation = actual - prediction,
            "3P%" = threePct,
            propensity = deviation * (threePct / 100)^3 )


# model performance --------------------------------------------------

preds_df %>%
     select(RSquared, RMSE) %>% 
     gather() %>% 
ggplot(aes(x = value)) +
     geom_histogram(bins = 15) +
     facet_grid(~ key,
                scales = 'free_x') +
     labs(y = 'Count',
          x = 'Value') +
     theme_bw() +
     theme(text = element_text(size = 20) )


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
     scale_y_continuous(breaks = 0:5, limits = c(0, 5.8)) +
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
     scale_y_continuous(breaks = -5:0, limits = c(-5.8, 0)) +
     coord_flip() +
     labs(x = '',
          y = 'Deviation From Expected 3PA',
          title = 'Fewer Threes Than Expected') + 
     scale_fill_viridis(limits = c(21.4, 45.1)) +
     theme_bw() +
     theme(text = element_text(size = 20) )

# propensity ---------------------------------------------------------

propensity_frame <- preds_df %>% 
     select(player, propensity) %>% 
     arrange(propensity)

write.csv(propensity_frame, 'propensity_frame.csv')










