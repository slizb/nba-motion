###Make/Miss Model in H2O and XGBoost


library (dplyr)
library (ggplot2)
load("data/preppedData.RData")
train <- uptrain
train$target <- as.factor(train$target)
test$target <- as.factor(test$target)
table(train$target)
table(test$target)
train <- train %>% filter (elapsedTime > 5)  %>% filter(numberPasses >= 0) %>% filter (elapsedTime < 36) 
test <- test %>% filter (elapsedTime > 5) %>% filter(numberPasses >= 0) %>% filter (elapsedTime < 36) 
train$gameid <- NULL
test$gameid <- NULL
train_fe <- train[,2:4]/train[,7]
test_fe <- test[,2:4]/test[,7]
train_fe[,4:24] <- train[,5:25]
test_fe[,4:24]  <- test[,5:25]

train2 <- train_fe
test2 <- test_fe
train2$onBallPositionMatchup <- NULL
test2$onBallPositionMatchup <- NULL
train2$elapsedTime <- train$elapsedTime
test2$elapsedTime <- test$elapsedTime
train2$onBallPositionMatchup <- train$onBallPositionMatchup
test2$onBallPositionMatchup <- test$onBallPositionMatchup
train2$target <- train$target
test2$target <- test$target

library(corrplot)
M <- cor(train2[,1:20])
corrplot(M, method="circle")

mydata <- train
mydata$categ <- train$target


### BOX PLOTS
total<-NULL
data <- NULL
par(mfrow=c(3,3), mar=c(3, 3, 0.5, 0.5), mgp = c(1.5, 0.3, 0), tck = -0.01,
    oma=c(0, 0, 1, 0))
png('some_boxplots%d.png')
sapply(seq_along(mydata)[1:23], function(i) {
  y <- mydata[, i]
  boxplot(y ~ mydata$categ, outline=FALSE, ylab=names(mydata[i]), tck = 1.0, 
          names=c("Make","Miss"), las=1)
  points(y ~ jitter(as.numeric(mydata$categ), 0.5), 
         col=ifelse(mydata$categ==1, 'firebrick', 'slateblue'))
  test <- wilcox.test(y ~ mydata$categ)
  pvalue <- test$p.value
  pvalueformatted <- format(pvalue, digits=3, nsmall=2)
  print (names(mydata[i]))
  print (pvalue)
  mtext(paste(colnames(mydata)[i], " p = ", pvalueformatted), side=3, 
        line=0.5, at=0.9, cex = 0.6)  
})
dev.off()
hist(train$onBallHeightMismatch)
table(train$onBallPositionMatchup)
makes <- train %>% filter(target=="Make")
table(makes$onBallPositionMatchup)


##XGBOOST
library(xgboost)
response <- (as.numeric(train$target)-1)
train.data <- data.matrix(train2[,-(21:22)])

param <- list("objective" = "binary:logistic",
            #  max_depth = 8,
             "eval_metric" = "auc")
cv.nround <- 50
cv.nfold <- 5
bst <-xgb.cv(data=train.data,eta=0.05,  label=response,params = param,nthread=4,nfold = cv.nfold,nrounds = cv.nround,verbose = T)

 bst <- xgboost(data = train.data, label = response, max.depth = 4,
                eta = .05, nthread = 4, nround = 50, objective = "binary:logistic")

model <- xgb.dump(bst, with_stats = T)
names <- dimnames(data.matrix(train))[[2]]
importance_matrix <- xgb.importance(names, model = bst)
imp <- as.data.frame(importance_matrix)
importance_matrix <- data.table(importance_matrix)
xgb.plot.importance(importance_matrix[1:10,])
barplot(imp[,4],names.arg = imp[,1])

featureList <- names[c(-24,-25)]
featureVector <- c() 
for (i in 1:length(featureList)) { 
  featureVector[i] <- paste(i-1, featureList[i], "q", sep="\t") 
}
write.table(featureVector, "fmap.txt", row.names=FALSE, quote = FALSE, col.names = FALSE)
xgb.dump(model = bst, fname = 'xgb.dump', fmap = "fmap.txt", with_stats = TRUE)



###H2o
library(h2o)

h2o.init()
train.h2o <- as.h2o(train2)
test.h2o <- as.h2o(test2)
x = c(1:9,14,20:23)
x = 1:24
y= 25


bb.glm <- h2o.glm(x=x,y=y,
                  training_frame = train.h2o,
                  validation_frame = test.h2o,standardize = TRUE,
                  
                  family = "binomial")

h2o.performance(bb.glm, valid = TRUE)

bb.gbm <- h2o.gbm(x=x,y=y,
                  training_frame = train.h2o,
                  validation_frame = test.h2o,balance_classes = TRUE,
                  distribution = "bernoulli")

h2o.performance(bb.gbm, valid = TRUE)
h2o.performance(bb.gbm)
h2o.varimp(bb.gbm)

bb.rf <- h2o.randomForest(x=x,y=y,ntrees = 200,
                  training_frame = train.h2o,
                  validation_frame = test.h2o)

h2o.performance(bb.rf,valid = TRUE)
h2o.varimp_plot(bb.gbm)
