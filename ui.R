library(shiny)
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
  , sidebarPanel( conditionalPanel( 'input.dataSourceType == "upload"'
                                  , uiOutput("columnSelectorUI")
                                  )
                , conditionalPanel( 'input.dataSourceType == "generate"'
                                  , sliderInput( "globalScale"
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
                  
                                  , br()
                                  , actionButton("refreshData", "Refresh data")
                                  )
                , br()
                , helpText(toHTML("Version 0.4.  [Source code](https://github.com/whitwort/anovaPlayground) available on github."))
                )
  
    # Main tab panels
  , mainPanel(tabsetPanel(
      tabPanel( "Analysis"
              , div( class = "container-fluid" 
                   , div( class = "row-fluid"
                        , div( class = "span9"
                               
                               #Main plot area
                             , h4("Data")
                             , conditionalPanel( "input.plotPoint || input.plotBoxplot || input.plotDotplot" 
                                               , plotOutput("groupPlot")
                                               )
                             , conditionalPanel( "input.plotDensity"
                                               , plotOutput("densityPlot")
                                               )
                             )
                          
                        , div( class = "span3"
                             , br()
                             , div( class="well"
                                         
                                    # Visualization options
                                  , helpText("Data visualization")
                                  , checkboxInput("plotBoxplot", "Boxplots",      TRUE )
                                  , checkboxInput("plotPoint",   "Data points",   FALSE)
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

    , tabPanel( "Source data"
              , div( class = "container-fluid" 
                   , div( class = "row-fluid"
                        , div( class = "span5"
                             , selectInput( "dataSourceType"
                                          , "Run the ANOVA using"
                                          , choices = c( "Randomly generated data" = "generate"
                                                       , "An existing dataset"     = "upload"
                                                       )
                                          )
                             )
                        , div( class = "span7"
                             , wellPanel( conditionalPanel( 'input.dataSourceType == "generate"'
                                                          , selectInput( "numberGenerator"
                                                                       , "Sample from a"
                                                                       , choices = c( "Normal distribution"     = "rnorm"
                                                                                    , "Log-normal distribution" = "rlnorm"
                                                                                    , "Uniform distribution"    = "runif"
                                                                                    )
                                                                       )
                                                          , numericInput( "mean"
                                                                        , "Initial group mean"
                                                                        , value = 0
                                                                        )
                                                          , numericInput( "sd"
                                                                        , "Initial group standard deviation"
                                                                        , value = 1
                                                                        )
                                                          )
                                        , conditionalPanel( 'input.dataSourceType == "upload"'
                                                          , selectInput( "uploadType"
                                                                       , "Get data from a"
                                                                       , choices = c( "Class data set"  = "data"
                                                                                    , "Local file"      = "file"
                                                                                    , "Web address"     = "url"
                                                                                    )
                                                                       )
                                                          , conditionalPanel( 'input.uploadType == "url"'
                                                                            , textInput( "url", "URL")
                                                                            )
                                                          , conditionalPanel( 'input.uploadType == "file"'
                                                                            , fileInput( "dataFile"
                                                                                       , "Upload a file"
                                                                                       )
                                                                            )
                                                          , conditionalPanel( 'input.uploadType == "data"'
                                                                            , selectInput( "dataset"
                                                                                         , "Data set"
                                                                                         , choices = list.files("data/")
                                                                                         )
                                                                            )
                                                          , conditionalPanel( 'input.uploadType == "file" || input.uploadType == "url"'
                                                                            , checkboxInput( "fileHeader"
                                                                                           , "Dataset has a header row"
                                                                                           , TRUE
                                                                                           )
                                                                            , selectInput( "fileSeparator" 
                                                                                         , "Separator"
                                                                                         , choices = c( "Whitespace"  = ""
                                                                                                      , "Comma"       = ","
                                                                                                      , "Semicolon"   = ";"
                                                                                                      , "Tab"         = "\t"
                                                                                                      )
                                                                                         )
                                                                            , selectInput( "naStrings" 
                                                                                         , "Missing data"
                                                                                         , choices = c( "NA"          = "NA"
                                                                                                      , "ND"          = "ND"
                                                                                                      , "None"        = "None"
                                                                                                      , "null"        = "null"
                                                                                                      , "Empty text"  = ""
                                                                                                      )
                                                                                         )
                                                                            , selectInput( "fileQuote"
                                                                                         , "String quoting"
                                                                                         , choices = c( 'Double (")'  = "\""
                                                                                                      , "Single (')"  = "'"
                                                                                                      , "None"        = ""
                                                                                                      )
                                                                                         )
                                                                            )

                                                          )
                                        )
                             )
                        )
                     
                    , div( class = "row-fluid"
                         , wellPanel( conditionalPanel( 'input.dataSourceType == "generate"'
                                                      , tableOutput('generatedData')
                                                      )
                                    , conditionalPanel( 'input.dataSourceType == "upload"'
                                                      , htmlOutput('loadDataError')
                                                      , dataTableOutput('userTable')
                                                      )
                                    )
                         )
                    )
              )
      
#       
#     , tabPanel( "Assumptions"
#               )
    
    , tabPanel( "Description"
              , helpText(toHTML("DESCRIPTION.md"))
              )
    ))
    
))
