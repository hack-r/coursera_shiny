## File: analysis.R
## Desc: This is the pre-made analysis 
## Copyright: (c) 2014, Jason D. Miller

# Libraries ---------------------------------------------------------------
require(caret)
require(randomForest)
require(stringr)

# Analysis ----------------------------------------------------------------
rf          <- randomForest(x = x, y = y2, ntree = 50)
predictions <- predict(rf, newdata=training)

confusionMatrix(predictions, training$answers.binary)
