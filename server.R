library(shiny)
library(plyr)
library(ggplot2)

# Scaling functions factored out here for clarity
scaleGroupMeans <- function(d, scalingFactor) {
  
  globalMean <- mean(d$response)
  
  if (scalingFactor > 0) { 
    multiplier <- scalingFactor
  } else {
    multiplier <- scalingFactor / 10
  }
  
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
          x$response  <- x$response - deltas
         
          return( x )
          
       })
  
}

responseLimits <- function(d) {
  
  maxScaling <- scaleGroupMeans(d, 10)$response
  
  c( min(maxScaling) 
   , max(maxScaling)
   )

}

# Define the server-side logic that updates data and plots in response to user
# input
shinyServer(function(input, output) {
  
  # The data source for our ANOVA is initially created as a matrix
  createMatrix  <- reactive({
    
    # Re-run this expression whenever the refresh button is clicked
    refresh <- input$refreshData
    
    # Replicate will return a matrix with one column of data for each group
    replicate( n    = input$groups
               
               # We'll use a random number generator pulling from a normal
               # distribution to create the data for each column
             , expr = rnorm( input$n 
                           , mean = input$mean
                           , sd   = input$sd
                           )
             )

  })
  
  sourceTable   <- reactive({
    
    data.frame( group     = rep(LETTERS[1:input$groups], each = input$n)
              , response  = c(createMatrix()) 
              )
  
  })
  
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
  
  # Re-run the ANOVA when the source data changes
  fitANOVA      <- reactive({
    
    # We'll use the aov wrapper which fits a GLM and then runs an ANOVA
    aov(response ~ group, data = scaledTable())
    
  })
  
  # The render* functions produce results shown in the client
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
                           ) + theme( legend.position = "none" )
    }
    
    if (input$plotPoint) {
      p <- p + geom_point()
    }
    
    if (input$plotScale == FALSE) {
      
      p <- p + ylim( responseLimits(sourceData) )
      
    }
    
    # Send the final plot to the client
    print(p)
    
  })
  
  output$densityPlot  <- renderPlot({
    
    sourceData <- sourceTable()
    scaledData <- scaledTable()
    
    p <- ggplot(scaledData, aes(response, fill = group)) + geom_density(alpha = 0.3)
    
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
  
  output$summary      <- renderPrint({
    
    # Get the latest ANOVA fit and print summary text
    fit <- fitANOVA()
    summary(fit)
    
  })
  
  output$distPlot   <- renderPlot({
    
    # Get the lastest ANOVA fit
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
      polygon <- rbind(c(f.value, 0), subset(d, x >= f.value), c(d[nrow(d), "X"]), c(5,0))
      p <- p + geom_polygon(data = polygon, aes(x, y))
    }
    
    print(p)
    
  })
  
})