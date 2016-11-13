#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

source("R_FinanceData.R")
stockIndex <- "FTSE"
indexMembers <- GetIndexMembers(stockIndex)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  values <- reactiveValues(default = 0)
  
  observeEvent(input$cmdRefresh,{
    values$default <- input$cmdRefresh
  })
  
  
  indexSummary.DT <- eventReactive(input$cmdRefresh , {
    
    indexSummary <- getQuote(indexMembers$Symbol, src="yahoo")
    indexSummary$Name <- indexMembers[match(row.names(indexSummary), indexMembers$Symbol), "Name"]
    indexSummary <- indexSummary[, c( ncol(indexSummary), 1:(ncol(indexSummary)-1) ) ]
    indexSummary.DT <- DT::datatable(indexSummary ,
             options = list(
                     order =  list(5, 'desc')))
    
    indexSummary.DT$x$data[, "% Change"] <- as.numeric(sub("%","",indexSummary.DT$x$data[, "% Change"]))/100
    formatPercentage(indexSummary.DT, "% Change", 2)
 
  
    
  })
  
  output$timeSeriesPlot <- renderPlot({
    
     getSymbols(input$lstStock, src="yahoo")
    
      tempTS <- get(input$lstStock)
      tempTS <- tempTS[ paste0(input$dtRange, collapse = "/")]
    
      plot(tempTS, main=input$lstStock )
    
  })
  
  output$membersTable <- DT::renderDataTable({
    if(values$default == 0){
      indexSummary <- getQuote(indexMembers$Symbol, src="yahoo")
      indexSummary$Name <- indexMembers[match(row.names(indexSummary), indexMembers$Symbol), "Name"]
      indexSummary <- indexSummary[, c( ncol(indexSummary), 1:(ncol(indexSummary)-1) ) ]
      indexSummary.DT <- DT::datatable(indexSummary ,
                                       options = list(
                                         order =  list(5, 'desc')))
      
      indexSummary.DT$x$data[, "% Change"] <- as.numeric(sub("%","",indexSummary.DT$x$data[, "% Change"]))/100
      formatPercentage(indexSummary.DT, "% Change", 2)
      
    }
    
    else{
      indexSummary.DT()
    }
    
    
  })
  
})
