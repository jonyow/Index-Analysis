#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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


# indexSummary <- getQuote(indexMembers$Symbol, src="yahoo")
# indexSummary.DT <- DT::datatable(indexSummary)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Index Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
       
       selectInput("lstStock", "Choose Stock:", indexMembers$Symbol, indexMembers$Symbol[1], FALSE, width=150),
       
       dateRangeInput("dtRange", "Select Date Range:", "2007-01-01", Sys.Date(), "2007-01-01", Sys.Date()),
       
       selectInput("lstSource", "Choose Source:", c("google", "yahoo"), "google",  FALSE, width=150),
       
       actionButton("cmdRefresh",
                    "Refresh Table", width=150) 
       
       
    , width=3),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("timeSeriesPlot"),
       
       dataTableOutput("membersTable")
       
    
       
    )
  )
))
