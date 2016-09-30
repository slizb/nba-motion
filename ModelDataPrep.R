
library(dplyr)
library(caret)
library(readr)

rm(list=ls())

set.seed(321)

df <- read.csv('event_features_9-20-16.csv') %>% 
     .[complete.cases(.),]


model_df <- df %>% 
     mutate(target = ifelse(EVENTMSGTYPE == 1, 'Make', 'Miss'),
            target = as.factor(target),
            team_id = as.factor(team_id),
            PLAYER1_ID = as.factor(PLAYER1_ID)) %>% 
     select(-c(X,
               EVENTMSGTYPE))

trainIndex <- createDataPartition(model_df$target, 
                                  p = .7,
                                  list = F,
                                  times = 1)

train <- model_df[ trainIndex,]
test  <- model_df[-trainIndex,]

up_train <- upSample(x = train %>% select(-target),
                     y = train$target)

save(train, test, up_train, file = "preppedData.RData")
