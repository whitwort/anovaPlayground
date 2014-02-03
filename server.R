library(shiny)
library(plyr)
library(ggplot2)

# # #
# Scaling functions
#
# After a bit of experimentation I settled on this approach to the scaling 
# functions.  The implementation is a bit non-intuitive, but this design gave
# the best tactile feel when playing with the sliders.
#
# Both sliders work on a -10 to 10 scale, but the behavior is fundamentally 
# different on either side of 0. Moving 0 > 10 amplifies distances by ~10x 
# (between groups) or ~2x (within group); moving 0 > -10 collapses data to their
# global (between group) or local (within group) means.
# 
# As of version ~0.2, both of the scaling functions operate on data.frame
# structures (d) allowing them to support unbalanced designs.
#
# The third function, responseLimits, simply projects the widest group-scaling
# data spread; it's used to pick response-axis limits.

scaleGroupMeans <- function(d, scalingFactor) {
  
  globalMean    <- mean(d$response)
  
  if (scalingFactor > 0) { 
    multiplier  <- scalingFactor
  } else {
    multiplier  <- scalingFactor / 10
  }
  
  # ddply from the plyr package; it takes a data.frame, splits it by .(group),
  # applies the given function, and returns a new data.frame
  ddply( d
       , .(group)
       , function(x) {
        
          groupDist   <- mean(x$response) - globalMean
          groupDelta  <- groupDist * multiplier
          x$response  <- x$response + groupDelta
          
          return( x )
         
       })
  
}

scaleGroupSDs <- function(d, scalingFactor) {
  
  if (scalingFactor > 0) {
    multiplier <- -1 * (scalingFactor / 5)
  } else {
    multiplier <- abs(scalingFactor) / 10
  }
  
  ddply( d
       , .(group)
       , function(x) {
         
          deltas      <- (x$response - mean(x$response)) * multiplier
          x$response  <-  x$response - deltas
         
          return( x )
          
       })
  
}

responseLimits <- function(d) {
  
  maxScaling <- scaleGroupMeans(d, 10)$response
  
  c( min(maxScaling) 
   , max(maxScaling)
   )

}


# # #
# Shiny server
#
# This large wrapper function defines the server-side logic for the web
# application.  It's a giant closure that scopes input and output bindings.  The
# input$ list will hold the latest data recieved from a client (a users's
# browser page) and the output$ list is used to push results back out to the
# client.  We don't have to worry about managing individual user sessions; shiny
# will take care of that for us because it's awesome.

shinyServer(function(input, output, session) {
  
  # # #
  # Reactive expressions
  #
  # The first set of data mappings that we'll define here are shiny reactive 
  # expressions ( created with reactive({...}) ).  Each reactive call evaluates
  # to a new closure we can use in the rest of our server code. The beauty of 
  # the design of shiny is that the return from these reactives will be cached
  # by the server and only updated when dependant input$ value(s) change.
  #
  # For us, this means that we'll end up with an efficient server as long as we
  # partition the core application logic into discrete reactive expressions that
  # have limited input$ dependencies and outputs that are useful to cache.
  # 
  
  # The first of two possible paths to source data for the ANOVA is a randomly
  # generated matrix
  
  createMatrix    <- reactive({
    
    # Re-run this expression whenever the refresh button is clicked
    refresh <- input$refreshData
    
    # Choose our sampling function from a predefined list (we never eval() data 
    # from a client for security reasons)
    sample = list( "rnorm"  = rnorm
                 , "rlnorm" = rlnorm
                 , "runif"  = function (n, mean, sd) { 
                                runif(n, mean - sd, mean + sd) 
                              }
                 )[[input$numberGenerator]]
    
    # Replicate will return a matrix with one column of data for each group
    replicate( n    = input$groups
             , expr = sample( input$n 
                            , mean = input$mean
                            , sd   = input$sd
                            )
             )

  })
  
  # The second possible path to source data is a table that the user has
  # uploaded
  parseUserData   <- reactive({
    
    # Here we define a simple read.table closure; it will throw errors if there
    # are any points of failure in loading or parsing user's data
    loadUserData <- function() {
      
      if (input$uploadType == 'url') {
        sourcePath <- input$url
      }
      if (input$uploadType == 'file') {
        sourcePath <- input$dataFile$datapath
      }
      if (input$uploadType == 'data') {
        sourcePath <- paste( "data", input$dataset, sep= "/")
      }
      
      read.table( file        = sourcePath
                , header      = input$fileHeader
                , sep         = input$fileSeparator
                , quote       = input$fileQuote
                , na.strings  = input$naStrings
                )
      
    }
    
    # Here we try loading and parsing the data given to us by the user.  If an
    # error is thrown, we simply return a text description of what went wrong. 
    # The value of this function will therefore be either: (a) a data.frame, or
    # (b) a character vector with error information (we'll test for this
    # below.)
    tryCatch( loadUserData()
            , error = function(e) { conditionMessage(e) }
            )
    
  })
  
  # The two paths to source data converge here.  We maintain two data.frame
  # objects: the source data and the scaled data.  This allows us to be smart
  # about response-axis scaling.
  sourceTable   <- reactive({
    
    # If we're loading user data, the parsing has gone well, and there is a
    # clear grouping column and response variable column, use this data.
    if ( input$dataSourceType == "upload" & 
         !is.null(input$groupColumn) & 
         !is.null(input$responseColumn)) {
      
      updateCheckboxInput(session, 'plotScale', value = TRUE)
      
      userData <- parseUserData()
      data.frame( group     = userData[[input$groupColumn]]
                , response  = userData[[input$responseColumn]]
                )
      
    # If we're generating random data, or any of the above failed, fall back on
    # our sampling matrix.
    } else {
      
      updateCheckboxInput(session, 'plotScale', value = FALSE)
      data.frame( group     = rep(LETTERS[1:input$groups], each = input$n)
                , response  = c(createMatrix()) 
                )
    
    }
  
  })
  
  # This closure returns the result of the current scaling settings.
  scaledTable   <- reactive({
    
    d  <- sourceTable()
    
    if (input$globalScale != 0) {
      d <- scaleGroupMeans(d, input$globalScale)
    }
    
    if (input$groupScale != 0) {
      d <- scaleGroupSDs(d, input$groupScale)      
    }
    
    return( d )
    
  })
  
  # We'll re-run the ANOVA whenever the scaling changes.
  fitANOVA      <- reactive({
    
    # We'll use the aov wrapper which fits a GLM and then runs an ANOVA
    aov(response ~ group, data = scaledTable())
    
  })
  
  
  # # #
  # output$ functions
  #
  # In this section of the server code we map special reactive expressions (the 
  # render* functions) to output$ bindings; this is the data that gets sent to 
  # clients. Like the reactive expressions above, these values are only 
  # recalculated when their inputs change.
  
  # The main group-response variable plot
  output$groupPlot    <- renderPlot({
    
    sourceData <- sourceTable()
    scaledData <- scaledTable()
    
    # We'll setup a ggplot2 plot with key aesthetic mappings and add geom_
    # elements based on the visualizations selected
    p <- ggplot(scaledData, aes(group, response))
    
    # The order of geom additions matters here because it determines what gets
    # plotted on each layer
    if (input$plotMean) {
      p <- p + geom_hline( yintercept = mean(scaledData$response)
                         , size       = 1
                         , colour     = "grey50"
                         )
    }
    
    if (input$plotMedian) {
      p <- p + geom_hline( yintercept = median(scaledData$response)
                         , size       = 1
                         , linetype   = 2
                         , colour     = "grey50" 
                         )
    }
    
    if (input$plotBoxplot) {
      p <- p + geom_boxplot( aes(fill = group, alpha = 0.8) ) + 
          theme( legend.position = "none" )
    }
    
    if (input$plotDotplot) {
      p <- p + geom_dotplot( binaxis  = "y"
                           , stackdir = "center"
                           , aes( fill =  group, alpha = 0.9 )
                           ) + 
          theme( legend.position = "none" )
    }
    
    if (input$plotPoint) {
      p <- p + geom_point()
    }
    
    if (input$plotScale == FALSE) {
      p <- p + ylim( responseLimits(sourceData) )
    }
    
    if (length(levels(scaledData$group)) > 10) {
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    }
    
    # We call print() to send the final plot to the client
    print(p)
    
  })
  
  # For now, we need to put the density plots in a separate visualization from
  # the above because it is non-trivial to mix x-axis scales in a single plot. 
  # I've tried to make the density plots observe as many of the groupPlots
  # settings as possible.
  output$densityPlot  <- renderPlot({
    
    sourceData <- sourceTable()
    scaledData <- scaledTable()
    
    p <- ggplot(scaledData, aes(response, fill = group)) + 
          geom_density(alpha = 0.3)
    
    if (input$plotMean) {
      p <- p + geom_vline( xintercept = mean(scaledData$response)
                         , size       = 1
                         , colour     = "grey50"
                         )
    }
    
    if (input$plotMedian) {
      p <- p + geom_vline( xintercept = median(scaledData$response)
                         , size       = 1
                         , linetype   = 2
                         , colour     = "grey50" 
                         )
    }
    
    if (input$plotScale == FALSE) {
      p <- p + xlim( responseLimits(sourceData) )
    }
    
    print(p)
    
  })
  
  # Here we send the actual ANOVA results to the client as a simple text block.
  output$summary      <- renderPrint({
    
    fit <- fitANOVA()
    summary(fit)
    
  })
  
  # This plot shows the F-distribution corresonding to the latest fit.
  output$distPlot   <- renderPlot({
    
    fit     <- fitANOVA()
    
    # This is ugly, but it will extract and round the F-stat from the GLM fit
    f.value <- round(summary(fit)[[1]][["F value"]][[1]], 1)
    
    # Create a little data.frame with values for the current F density
    x       <- seq(0, 6, 0.1)
    d       <- data.frame(x = x, y = df(x, input$groups - 1, fit$df.residual))
    
    # Draw the F-distrubution for the current ANOVA fit
    p <- ggplot(d, aes(x, y)) + geom_line() + 
          xlab("F-value") +
          ylab("Density")
           
    # If the F-value is low enough to visualize, we'll shade the area under the
    # F-dist curve for this probability
    if (f.value < 6) {
      polygon <- rbind( # The lower left corner of the shaded polygon
                        c(f.value, 0)
                        
                        # The coordinates of the points along the f-dist curve
                      , subset(d, x >= f.value)
                      , c(d[nrow(d), "X"])
                        
                        # The lower right corner (given by cutoffs above)
                      , c(5,0))
      p       <- p + geom_polygon(data = polygon, aes(x, y))
    }
    
    print(p)
    
  })
  
  # User feedback on the current source data
  output$generatedData <- renderTable({ createMatrix() })
  output$loadDataError <- renderUI({
    userData <- parseUserData()
    if ( is.data.frame(userData) ) {
      HTML( renderText("")() )
      
    } else {
      
      # We'll default to a message generated in base R
      message <- paste( "I couldn't parse your table because:"
                        , br()
                        , div(class = "shiny-output-error", userData)
                        , br()
                        , "Try changing the separator or quoting options above."
      )
      
      # More user-friendly error messages...
      if ( input$uploadType == 'url' & input$url == "" ) {
        message <- "Enter a url..."
      }
      if ( input$uploadType == 'file' & is.null(input$dataFile) ) {
        message <- "Upload a file..."
      }
      
      # Send out the result
      HTML( renderText(message)() )
      
    }
  })
  output$userTable <- renderDataTable({
    parseUserData()
  })
  
  # Generates selection controls to allow the user to choose a grouping and
  # response variable from their uploaded table
  output$columnSelectorUI <- renderUI({
    
    userData <- parseUserData()
    
    if ( is.data.frame(userData) ) {
      
      # I feel like there should be a more elegant way to do this...
      groupColumns    <- colnames(userData)[sapply(userData, is.factor)]
      responseColumns <- colnames(userData)[sapply(userData, is.numeric)]
      
      div( selectInput( "groupColumn"
                      , "Grouping variable"
                      , groupColumns
                      )
         , selectInput( "responseColumn"
                      , "Response variable"
                      , responseColumns
                      )
         )
    }
    
  })
  
  # t-test panel
  groupLevels  <- reactive({
    scaledData <- scaledTable()
    levels(scaledData$group)
  })
  output$groupLevel1UI <- renderUI({
    div( class = "span5"
       , selectInput( "groupLevel1"
                    , "First group:"
                    , choices = groupLevels()
                    )
       )
  })
  output$groupLevel2UI <- renderUI({
    div( class = "span5"
       , selectInput( "groupLevel2"
                    , "Second group:"
                    , choices = setdiff(groupLevels(), input$groupLevel1)
                    )
       )
  })
  twoSampleData <- reactive({
    scaledData <- scaledTable()
    scaledData[ scaledData$group == input$groupLevel1 | scaledData$group == input$groupLevel2 , ]
  })
  output$tDensityPlot <- renderPlot({
    p <- ggplot(twoSampleData(), aes(response, fill = group)) + 
      geom_density(alpha = 0.3)
    print(p)
  })
  output$tSummary     <- renderPrint({
    t.test(response~group, data = twoSampleData())
  })
  
  
  # regression panel
#   output$response1UI <- renderUI({
#     div( class = "span5"
#        , selectInput( "response1"
#                     , "First response:"
#                     , choices = 
#                     )
#        )
#   })
#   output$response1UI <- renderUI({
#     div( class = "span5"
#        , selectInput( "response2"
#                     , "Second response:"
#                     , choices = 
#                     )
#        )
#   })
#   regressionData <- reactive({
#     scaledData <- scaledTable()
#     scaledData[ scaledData$group == input$groupLevel1 | scaledData$group == input$groupLevel2 , ]
#   })
#   output$regressionPlot <- renderPlot({
#     p <- ggplot(twoSampleData(), aes(response, fill = group)) + 
#       geom_density(alpha = 0.3)
#     print(p)
#   })
#   output$regressionSummary     <- renderPrint({
#     t.test(response~group, data = twoSampleData())
#   })
    
  
})