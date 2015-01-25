## File: analysis.R
## Desc: This is the pre-made analysis 
## Copyright: (c) 2014, Jason D. Miller

# Libraries ---------------------------------------------------------------
require(caret)
require(plyr)
require(randomForest)
require(ROCR)
require(stringr)

# Data --------------------------------------------------------------------

# XPath Stackoverflow data scrape of questions tagged with R
data <- read.csv("stackoverflow.csv") 

# Rm NA's
data$link_title <- NULL
data$reputation[is.na(data$reputation)] <- 0

# Transform badges string into structured data
data$badgestr    <- as.character(data$badges)
data$badgestr    <- gsub(";", "", data$badgestr)
badgesplit      <- str_split_fixed(data$badgestr, " ", 3)
data$bron_badges <- badgesplit[,1]
data$silv_badges <- badgesplit[,2]
data$gold_badges <- badgesplit[,3]
data$bron_badges[data$bron_badges == ""] <- 0
data$silv_badges[data$silv_badges == ""] <- 0
data$gold_badges[data$gold_badges == ""] <- 0
data$bron_badges <- as.numeric(data$bron_badges)
data$silv_badges <- as.numeric(data$silv_badges)
data$gold_badges <- as.numeric(data$gold_badges)

#Split tags apart
data$tags <- as.character(data$tags)
data$tags <- gsub(" ", "", data$tags)
sotags    <- unique(unlist(strsplit(data$tags, ";")))
data$id   <- 1:nrow(data)
tag.data  <- ddply(data, .(id), function(x)
  table(factor(unlist(strsplit(x$tags, ";")),
               levels = sotags)))
x.mean   <- colMeans(tag.data[-1])
tagmeans <- tag.data[,c('id', names(sort(x.mean, decreasing=TRUE)))] 
data     <- cbind(data, tagmeans) 

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
dat$badges         <- NULL
dat$badgestr       <- NULL
dat$rownum         <- NULL
dat$id             <- NULL
dat$id.1           <- NULL

# Fix age variable
# for( i in 1:nrow(dat)){
#   if(grep("mins",dat$age[1]))
# }
dat$age <- NULL

# Partition Data
idx      <- createDataPartition(y=dat$answers, p=0.6, list=FALSE )
training <- dat[idx,]
testing  <- dat[-idx,]
x        <- training[-ncol(training)]
y        <- training$answers

# Fix a few records
x[,2][is.na(x[,2])] <- 0

# Make a binary version of the outcome
training$answers.binary                              <- training$answers
training$answers.binary[training$answers.binary > 0] <- 1
testing$answers.binary                               <- testing$answers
testing$answers.binary[testing$answers.binary > 0]   <- 1
y            <- as.factor(y)
y2           <- as.numeric(y)
y2[y2 == 1]  <- 0
y2[y2 > 1]   <- 1
y2           <- as.factor(y2)

# Analysis ----------------------------------------------------------------
rf          <- randomForest(x = x, y = y2, ntree = 50)
predictions <- predict(rf, newdata = x)
pred        <- predict(rf, type = "prob")
confusionMatrix(predictions, training$answers.binary) #88.04% accuracy


# GLM ---------------------------------------------------------------------
fit <- glm(answers ~ votes + reputation + views + I(bron_badges + silv_badges 
                           + gold_badges) + I(training$ggplot2 + training$r +
#                                                 training$mathjax +
#                                                 training$freeze + training$rdp +
#                                                 training$chunks + training$python +
                                                training$ruby),  
                                              data = training)
summary.glm(fit)
saveRDS(fit, file = "ordered_logit.rds")
saveRDS(training, file = "training.rds")

# Testing the Algorithm ---------------------------------------------------
validation <- predict(rf, newdata = testing)

confusionMatrix(validation, testing$answers.binary) #81.49%
saveRDS(rf, file = "rf.rds")

# Viz ---------------------------------------------------------------------
preds <- prediction(as.numeric(predictions), y2)
perf  <- performance(preds, "tpr", "fpr")
plot(perf,col='red',lwd=3)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
saveRDS(perf, file = "perf.rds")

# Alternate Analysis (regular logit) --------------------------------------
# glmdata <- cbind(x, y)
# fit <- glm(y~ ., data = glmdata, family = binomial(link = "logit"))
# summary.glm(fit)
# nullmod <- glm(y~ 1, data = glmdata, family = binomial(link = "logit"))
# 1-logLik(fit)/logLik(nullmod)
# #'log Lik.' 0.6693239 (df=647)
# saveRDS(fit, file = "fit.rds")
