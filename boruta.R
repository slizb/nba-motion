
library(caret)
library(Boruta)
library(ranger)
library(pROC)
library(ggplot2)
library(dplyr)

set.seed(321)

B <- Boruta(as.factor(target) ~ .,
            data = model_df,
            doTrace = 2,
            num.threads = 4,
            respect.unordered.factors = T)

par(mar = c(11.5, 3, 1, 1))
plot(B, las = 2, xlab = '')

# normalize ?
# boxcox ?

#



