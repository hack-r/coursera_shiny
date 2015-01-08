## File: server.R
## Desc: This is the ui of my Coursera class project shiny app
## Copyright: (c) 2014, Jason D. Miller

require(RCurl)
require(googleVis)
library(shiny)
require(shinyapps)
so   <- getURL("http://wikisend.com/download/725112/stackexchange.csv")
data <- read.csv(so) 
shinyServer(function(input, output) {
  output$population <- renderText({paste("The selected population is",
                                         paste(input$select_pop)) })
  output$results <- renderGvis({gvisPieChart(piedata)})
})