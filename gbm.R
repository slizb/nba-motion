

library(doMC)
registerDoMC(cores = 4)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3,
                           search = 'random',
                           classProbs = TRUE)

set.seed(321)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit1 <- train(Class ~ ., 
                 data = up_train,
                 method = "gbm",
                 trControl = fitControl,
                 metric = "ROC",
                 tuneGrid = gbmGrid,
                 verbose = FALSE,
                 tuneLength = 10)

# ggplot(gbmFit1, metric = "Kappa")
# 
# ggplot(gbmFit1) +
#      geom_smooth(se = FALSE, span = .8, method = loess) +
#      theme(legend.position = "top")

preds <- predict(gbmFit1, test, type = "prob")

roc_df <- data.frame(actual = as.factor(test$target),
                     prediction = as.numeric(preds[,1]))

my_roc <- pROC::roc(actual ~ prediction,
                    roc_df,
                    ci = T,
                    plot = T,
                    print.auc = T)

plot(my_roc,
     print.auc = T)
