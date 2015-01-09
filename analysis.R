## File: analysis.R
## Desc: This is the pre-made analysis 
## Copyright: (c) 2014, Jason D. Miller


# Libraries ---------------------------------------------------------------
require(caret)
require(randomForest)
require(stringr)

# Data --------------------------------------------------------------------
# Load raw data via server.R

# Get response variable into last column, get rid of NA's 
answers <- data$answers
n       <- which(names(data) == "answers")
data2   <- data[, -n]
dat     <- cbind(data2,answers)

# Get rid of unwanted columns
dat$page           <- NULL
dat$result_1_to_50 <- NULL
dat$resultpage_URI <- NULL
dat$link           <- NULL
dat$link_source    <- NULL
dat$tag            <- NULL
dat$question       <- NULL
dat$title          <- NULL
dat$tags           <- NULL

# Fix age variable
for( i in 1:nrow(dat)){
 grep("mins",dat$age[i])
}

# Partition Data
idx      <- createDataPartition(y=dat$answers, p=0.6, list=FALSE )
training <- dat[idx,]
testing  <- dat[-idx,]
x        <- training[-ncol(training)]
x        <- as.matrix(x)
y        <- training$answers

# Analysis ----------------------------------------------------------------
rf <- randomForest(x = x, y = y, ntree = 500)
predictions <- predict(rf, newdata=training)
confusionMatrix(predictions, training$answers)