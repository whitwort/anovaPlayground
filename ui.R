library(shiny)
library(shinyIncubator)
library(markdown)

# Helper functions
toHTML <- function(s) {
  
  if (file.exists(s)) {
    toHTML(paste(readLines(s), collapse="\n"))
  } else {
    HTML(markdownToHTML(text = s, fragment.only = TRUE))
  }
  
}

# Define UI layout for the client application
shinyUI(pageWithSidebar(
  
    # Application title
    headerPanel("ANOVA Playground")
  
    # Sidebar with data transformation controls
  , sidebarPanel( sliderInput( "globalScale"
                             , "Adjust variation between groups"
                             , value = 0
                             , min   = -10
                             , max   = 10
                             , step  = 1
                             )
                , br()
                , sliderInput( "groupScale" 
                             , "Adjust variation within groups"
                             , value = 0
                             , min   = -10
                             , max   = 10
                             , step  = 1
                             )
                , br()
                , sliderInput( "groups"
                             , "Number of groups"
                             , value = 5
                             , min   = 2
                             , max   = 20
                             , step  = 1
                             )
                , br()
                , numericInput( "n"
                              , "Group sample size"
                              , value = 10
                              , min   = 0
                              )
                , numericInput( "mean"
                              , "Initial group mean"
                              , value = 0
                              )
                , numericInput( "sd"
                              , "Initial group standard deviation"
                              , value = 1
                              )
                , br()
                , actionButton("refreshData", "Refresh data")
                , br(), br()
                , helpText(toHTML("Version 0.1.  [Source code](https://github.com/whitwort/anovaPlayground) available on github."))
                )
  
    # Main tab panels
  , mainPanel(tabsetPanel(
      tabPanel( "Analysis"
              , div( class = "container-fluid" 
                   , div( class = "row-fluid"
                        , div( class = "span9"
                               
                               #Main plot area
                             , h4("Data")
                             , conditionalPanel( condition = "input.plotPoint || input.plotBoxplot || input.plotDotplot" 
                                               , plotOutput("groupPlot")
                                               )
                             , conditionalPanel( condition = "input.plotDensity"
                                               , plotOutput("densityPlot")
                                               )
                             )
                          
                        , div( class = "span3"
                             , br()
                             , div( class="well"
                                         
                                    # Visualization options
                                  , helpText("Data visualization")
                                  , checkboxInput("plotPoint",   "Data points",   TRUE )
                                  , checkboxInput("plotBoxplot", "Boxplots",      FALSE)
                                  , checkboxInput("plotDotplot", "Dotplots",      FALSE)
                                  , checkboxInput("plotMean",    "Global mean",   FALSE)
                                  , checkboxInput("plotMedian",  "Global median", FALSE)
                                  , checkboxInput("plotScale",   "Scale y-axis",  FALSE)
                                  , checkboxInput("plotDensity", "Density plots", FALSE)
                                  )
                             )
                        )
                   , div( class = "row-fluid"
                        , h4("One-way ANOVA")
                        , verbatimTextOutput("summary")
                        , plotOutput("distPlot")
                        )
                   )
              )

#     , tabPanel( "Source data"
#               )
#       
#     , tabPanel( "Assumptions"
#               )
    
    , tabPanel( "Description"
              , helpText(toHTML("DESCRIPTION.md"))
              )
    ))
    
))
