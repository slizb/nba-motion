
library(caret)
library(Boruta)
library(ranger)
library(pROC)
library(ggplot2)
library(dplyr)

load('preppedData.RData')

set.seed(321)

# run rf with varying vi measures ------------------------------------

# gini index
rf1 <- ranger(as.factor(target) ~ .,
              data = train,
              num.threads = 4,
              write.forest = T,
              #respect.unordered.factors = T,
              probability = T,
              importance = 'impurity')

par(mar = c(11.5, 3, 2, 1))
barplot(sort(rf1$variable.importance),
        las = 2,
        main = 'Impurity (Gini Index)')

rf2 <- ranger(as.factor(target) ~ .,
              data = train,
              num.threads = 4,
              write.forest = T,
              #respect.unordered.factors = T,
              probability = T,
              importance = 'permutation')

par(mar = c(11.5, 4, 2, 1))
barplot(sort(rf2$variable.importance),
        las = 2,
        main = 'Permutation')


preds <- predict(rf, test)

roc_df <- data.frame(actual = as.factor(test$target),
                     prediction = as.numeric(preds$predictions[,1]))

my_roc <- pROC::roc(actual ~ prediction,
                    roc_df,
                    ci = T,
                    plot = T,
                    print.auc = T)

plot(my_roc,
     print.auc = T)

