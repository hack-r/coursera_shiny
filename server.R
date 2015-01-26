## File: server.R
## Desc: This is the ui of my Coursera class project shiny app
## Copyright: (c) 2014, Jason D. Miller

# Libraries
require(data.table)
require(randomForest)
require(ROCR)
require(shiny)
require(shinyapps)
require(stringr)

# Run shinyServer function
shinyServer(function(input, output) {
  x      <- readRDS("x.rds")
  rf     <- readRDS("rf.rds")
  glm    <- readRDS("fit.rds")
  perf   <- readRDS("perf.rds")
  ol     <- readRDS("ordered_logit.rds")
  train  <- readRDS("training.rds")
  userdf <- x[1,]
  
  output$viz   <- renderPlot({plot(perf,col='red',lwd=3)
                              abline(a=0,b=1,lwd=2,lty=2,col="gray")
  })

  # Reactively update the prediction dataset!
  values <- reactiveValues()
  values$df <- userdf
  newEntry <- observe({
      values$df$bron_badges <- input$bron_badges
      values$df$silv_badges <- input$silv_badges
      values$df$gold_badges <- input$gold_badges
      values$df$reputation  <- input$reputation
      values$df$views       <- input$views
      values$df$votes       <- input$votes
  })
  output$table <- renderTable({data.frame(values$df)})

  output$results <- renderPrint({
                      {  ds1        <- values$df 
                         a <- predict(ol, newdata = data.frame(ds1))
                         names(a) <- NULL
                         cat(a)

                      }
                      })
})  