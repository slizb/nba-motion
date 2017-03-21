
library(caret)
library(Boruta)
library(ranger)
library(pROC)
library(ggplot2)
library(dplyr)

rm(list = ls())

load('preppedData.RData')

set.seed(321)

train <- train %>% 
     select( -c(gameid, playid)) %>% 
     mutate(stephenCurry = ifelse(PLAYER1_ID == '201939', 1, 0)) %>% 
     filter(timeInPaint > 0, 
            abs(timeInPaint) < 30 )

up_train <- up_train %>% 
     select( -c(gameid, playid)) %>% 
     mutate(stephenCurry = ifelse(PLAYER1_ID == '201939', 1, 0)) %>% 
     filter(timeInPaint > 0, 
            abs(timeInPaint) < 30,
            elapsedTime > 5)

test <- test %>% 
     filter(timeInPaint > 0,
            abs(timeInPaint) < 30)

# run rf with varying vi measures ------------------------------------

# gini index
rf1 <- ranger(Class ~ .,
              data = up_train,
              num.threads = 4,
              write.forest = T,
              #respect.unordered.factors = T,
              probability = T,
              importance = 'impurity')

par(mar = c(11.5, 3, 2, 1))
barplot(sort(rf1$variable.importance),
        las = 2,
        main = 'Impurity (Gini Index)')

# rf2 <- ranger(as.factor(target) ~ .,
#               data = train,
#               num.threads = 4,
#               write.forest = T,
#               #respect.unordered.factors = T,
#               probability = T,
#               importance = 'permutation')
# 
# par(mar = c(11.5, 4, 2, 1))
# barplot(sort(rf2$variable.importance),
#         las = 2,
#         main = 'Permutation')


preds <- predict(rf1, test)

roc_df <- data.frame(actual = as.factor(test$target),
                     predictionMake = as.numeric(preds$predictions[,1]),
                     predictionMiss = as.numeric(preds$predictions[,2]) )

roc_df$prediction <- ifelse(roc_df$predictionMake > roc_df$predictionMiss,
                            'Make',
                            'Miss') 

my_roc <- pROC::roc(actual ~ predictionMake,
                    roc_df,
                    ci = T,
                    plot = T,
                    print.auc = T)

plot(my_roc,
     print.auc = T)

table(roc_df$actual, roc_df$prediction)


