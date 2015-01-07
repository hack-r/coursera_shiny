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
  titlePanel("Coursera Class Project Shiny App",
             
             windowTitle = "Under Development"),
  
  # Sidebar to select options
  sidebarLayout(
    sidebarPanel(
#       img(src="",
#           width = "200px", height = "200px"),
      h4("Options"),
      radioButtons("radio", label = "Select Analysis",
                   choices = list("Descriptive Statistics",
                                  "Visualization")
      ),
      
      hr(),
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 100,
                     end = Sys.Date()-14),
      hr(),
      selectInput("select_pop", label = "Population",
                  choices = list("All",
                                 "New")),
      
      br(),
      br(),
      br(),
      br(),
      br()
    ),
    
    # Main Panel for displaying results
    mainPanel(
      tabsetPanel(
        tabPanel("Info", textOutput("population")),
        tabPanel("Results", htmlOutput("results"))
        
      )
    )
  )
    ))