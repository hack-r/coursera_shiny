## File: analysis.R
## Desc: This is the pre-made analysis 
## Copyright: (c) 2014, Jason D. Miller

# Libraries ---------------------------------------------------------------
require(caret)
require(randomForest)
require(stringr)

# Analysis ----------------------------------------------------------------
rf          <- randomForest(x = x, y = y2, ntree = 50)
predictions <- predict(rf, newdata = training)

confusionMatrix(predictions, training$answers.binary) #87.2% accuracy

# Testing the Algorithm ---------------------------------------------------
validation <- predict(rf, newdata = testing)

confusionMatrix(validation, testing$answers.binary) #81%

# Comment: Not bad, though not stellar performance. We can improve the performance
#                 by coding a structured version of the "age" variable, doing
#                 text-mining on the question titles and bodies, and/or 
#                 increasing the number of trees, as time allows


# Save the Object ---------------------------------------------------------
saveRDS(rf, file = "rf.rds")
