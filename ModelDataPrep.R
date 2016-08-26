
library(dplyr)
library(caret)

set.seed(321)

threes <- read.csv('/volumes/nba/threes.csv')

feats <- read.csv('event_features_7-17-16.csv')

df <- threes %>% 
     left_join(feats, 
               by = c('gameid' = 'gameid',
                      'event.id' = 'event.id')) %>% 
     .[complete.cases(.),]

model_df <- df %>% 
     mutate(target = ifelse(EVENTMSGTYPE == 1, 'Make', 'Miss')) %>% 
     select(-c(lastnameshooter, 
               teamName,
               id,
               EVENTNUM,
               EVENTMSGTYPE,
               EVENTMSGACTIONTYPE,
               event.id,
               PLAYER1_ID,
               gameid,
               team_off,
               team_def))

trainIndex <- createDataPartition(model_df$target, 
                                  p = .6,
                                  list = FALSE,
                                  times = 1)

train <- model_df[ trainIndex,]
test  <- model_df[-trainIndex,]

save(train, test, file = "preppedData.RData")
