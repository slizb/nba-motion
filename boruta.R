
library(caret)
library(Boruta)
library(ranger)
library(pROC)
library(ggplot2)

threes <- read.csv('/volumes/nba/threes.csv')

feats <- read.csv('event_features_7-17-16.csv')

df <- threes %>% 
     left_join(feats, 
               by = c('gameid' = 'gameid',
                      'event.id' = 'event.id')) %>% 
     .[complete.cases(.),]

model_df <- df %>% select(-c(lastnameshooter, 
                             teamName,
                             id,
                             EVENTNUM,
                             EVENTMSGACTIONTYPE,
                             event.id,
                             PLAYER1_ID,
                             gameid,
                             team_off,
                             team_def))

trainIndex <- createDataPartition(model_df$EVENTMSGTYPE, 
                                  p = .6,
                                  list = FALSE,
                                  times = 1)

train <- model_df[ trainIndex,]
test  <- model_df[-trainIndex,]


B <- Boruta(as.factor(EVENTMSGTYPE) ~ .,
            data = model_df,
            doTrace = 2,
            num.threads = 4 )

par(mar = c(11.5, 3, 1, 1))
plot(B, las = 2, xlab = '')

# normalize ?
# boxcox ?

#
rf <- ranger(as.factor(EVENTMSGTYPE) ~ .,
             data = train,
             num.threads = 4,
             write.forest = T)

preds <- predict(rf, test)

roc_df <- data.frame(actual = as.numeric(test$EVENTMSGTYPE),
                     prediction = as.numeric(preds$predictions))

my_roc <- roc(actual ~ prediction, roc_df)







