## File: server.R
## Desc: This is the ui of my Coursera class project shiny app
## Copyright: (c) 2014, Jason D. Miller

# Tell me if we need to refresh the pre-made analysis
refresh <- FALSE

# Libraries
require(data.table)
require(plyr)
require(shiny)
require(shinyapps)
require(stringr)

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
tags      <- unique(unlist(strsplit(data$tags, ";")))
data$id   <- 1:nrow(data)
tag.data  <- ddply(data, .(id), function(x)
             table(factor(unlist(strsplit(x$tags, ";")),
                 levels = tags)))
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
#x        <- as.matrix(x)
y        <- training$answers

# Run shinyServer function
shinyServer(function(input, output) {
  output$analysis <- renderText({paste("The selected analysis is",
                                         paste(input$select_analysis)) })
  
})