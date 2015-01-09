## File: server.R
## Desc: This is the ui of my Coursera class project shiny app
## Copyright: (c) 2014, Jason D. Miller

# Tell me if we need to refresh the pre-made analysis
refresh <- FALSE

# Libraries
require(data.table)
require(manipulate)
require(plyr)
require(shiny)
require(shinyapps)
require(stringr)

if(refresh == TRUE){
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
  x[,3][is.na(x[,3])] <- 0
  
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
}

# Read in RF --------------------------------------------------------------
rf <- readRDS("rf.rds")

# Run shinyServer function
shinyServer(function(input, output) {
  output$info  <- renderText({paste("The selected task is", 
                                   paste(input$select_task))})
  output$info1 <- renderText({paste("Model accuracy is 81%")})
  output$info2 <- renderText({paste("A ROC plot is available in the Visualization tab")})
  output$info3 <- renderText({paste("The selected plot is:", paste(input$plot_options))})
  output$info4 <- renderText({paste(input$sotags, collpase = " ", sep = ",")})                                   
  output$table <- renderTable({ data.frame(input$numbron, input$numsilv,
                                           input$numgold, input$numrep,
                                           input$numviews, 
                                           input$numvotes, t(input$sotags)
                                           )})
})  