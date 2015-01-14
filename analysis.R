## File: analysis.R
## Desc: This is the pre-made analysis 
## Copyright: (c) 2014, Jason D. Miller

# Libraries ---------------------------------------------------------------
require(caret)
require(randomForest)
require(ROCR)
require(stringr)

# Analysis ----------------------------------------------------------------
rf          <- randomForest(x = x, y = y2, ntree = 50)
predictions <- predict(rf, newdata = x)
pred        <- predict(rf, type = "prob")
confusionMatrix(predictions, training$answers.binary) #88.04% accuracy

# Testing the Algorithm ---------------------------------------------------
validation <- predict(rf, newdata = testing)

confusionMatrix(validation, testing$answers.binary) #81.49%
saveRDS(rf, file = "rf.rds")

# Viz ---------------------------------------------------------------------
preds <- prediction(as.numeric(predictions), y2)
perf  <- performance(preds, "tpr", "fpr")
viz <- plot(perf,col='red',lwd=3)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
saveRDS(viz, file = "viz.rds")

# Alternate Analysis (Regression) -----------------------------------------
glmdata <- cbind(x, y)
fit <- glm(y~ ., data = glmdata, family = binomial(link = "logit"))
summary.glm(fit)
nullmod <- glm(y~ 1, data = glmdata, family = binomial(link = "logit"))
1-logLik(fit)/logLik(nullmod)
#'log Lik.' 0.6693239 (df=647)
saveRDS(fit, file = "fit.rds")
