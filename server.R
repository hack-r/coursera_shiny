## File: server.R
## Desc: This is the ui of my Coursera class project shiny app
## Copyright: (c) 2014, Jason D. Miller

require(googleVis)
library(shiny)
require(shinyapps)

shinyServer(function(input, output) {
  output$population <- renderText({paste("The selected population is",
                                         paste(input$select_pop)) })
  output$results <- renderGvis({gvisPieChart(piedata)})
})