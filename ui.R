## File: ui.R
## Desc: This is the ui of my Coursera class project shiny app
## Copyright: (c) 2014, Jason D. Miller

library(shiny)


shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #48ca3b;
                    }
                    
                    "))
    ),
  # Application title
  titlePanel("Stackoverflow.com Answer Predictor for R Questions",
             windowTitle = "SO Answer Predictor"),
  h5("Coursera Developing Data Products class project"),
  
  # Sidebar to select options
  sidebarLayout(
    sidebarPanel(
#       img(src="",
#           width = "200px", height = "200px"),
      h4("Options"),

      selectInput("select_analysis", label = "Select Pre-made or Custom Analysis",
                  choices = list("Pre-made",
                                 "Custom")),
      conditionalPanel(
        condition = "input.select_analysis == 'Custom'", 
                    checkboxGroupInput('show_vars', 'Indepedent variables to use:',
                    names(dat))
        )
    ),
    
    # Main Panel for displaying results
    mainPanel(
      tabsetPanel(
        tabPanel("Info", textOutput("analysis")),
        tabPanel("Results", htmlOutput("results")),
        tabPanel("Visualization")
      )
    )
  )
    ))