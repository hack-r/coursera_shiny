## File: analysis.R
## Desc: This is the pre-made analysis 
## Copyright: (c) 2014, Jason D. Miller


# Libraries ---------------------------------------------------------------
require(caret)
require(randomForest)
require(stringr)

# Analysis ----------------------------------------------------------------
x[,3][is.na(x[,3])] <- 0
y <- as.factor(y)
rf <- randomForest(x = x, y = y, ntree = 500)
predictions <- predict(rf, newdata=training)
confusionMatrix(predictions, training$answers)
