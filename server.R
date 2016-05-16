#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dygraphs)
library(quantmod)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  
  output$time_series <- renderDygraph({
    
    stocks_load <- reactive({
      stocks <- strsplit(input$symbols," ")  %>% unlist()
      if(length(stocks)>0){
        stocks_close <- data.frame()
        for (n in stocks) {
          result <- getSymbols(n,from="2016-01-01",auto.assign = F)
          result <- result[,paste0(n,".Adjusted")]
          result <- (result - result[[1]]) / result[[1]] * 100
          
          # result <- paste0(round(result,2),"%")
          
          if(length(stocks_close)==0){
            stocks_close <- result
          } else {
            stocks_close <- cbind(stocks_close,result)
          }
        }
        
      }
      stocks_close
    })
    
    dygraph(stocks_load()) %>%
      dyRangeSelector() %>%
      dyHighlight( 
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = T,highlightSeriesOpts = list(highlightCircleSize = 5,strokeWidth = 3)) %>% 
      dyRoller(rollPeriod = 10) %>%
      dyLegend(show="onmouseover", width="1000",hideOnMouseOut=T)
    
  })
  
  output$max <- renderText({
    print("Hello")
  })
})
