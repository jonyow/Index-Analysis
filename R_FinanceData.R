
library(quantmod)
library("XML")

library("RCurl")

library(dplyr)



GetIndexMembers <- function(stockIndex){
  maxPageIndex <- 50
  indexTable <- NULL
  
  for(pageIndex in 0:maxPageIndex)
  {
    indexTableTemp <- GetIndexMembersForPageIndex(stockIndex, pageIndex)
    
    if( is.null(indexTableTemp))
    {
      indexTable <- indexTable[grepl("NaN N/A", indexTable$`Last Trade`, fixed=T)==FALSE ,]
       return(indexTable)
    }
    else
    {
        if(is.null(indexTable))
        {
          indexTable <- indexTableTemp
          
        }
      else
      {
        indexTable <- rbind(indexTable, indexTableTemp)
      }
      
    }
  }
  
  indexTable <- indexTable[grepl("NaN N/A", indexTable$`Last Trade`, fixed=T)==FALSE ,]
  
  return(indexTable)
  
  #grepl("NaN N/A", indexMembers$`Last Trade`, fixed=T)
  
  #filter(indexTable, 'Last Trade' != "NaN N/A")
  
}


GetIndexMembersForPageIndex <- function(stockIndex, pageIndex){
  
  webLinkInitial <- paste('https://uk.finance.yahoo.com/q/cp?s=%5E', stockIndex,  sep="")
  if(pageIndex>0)  {webLinkInitial <- paste(webLinkInitial, "&c=", pageIndex, sep="")}
  
  
  webHTML <- getURL(webLinkInitial)
  
  tableHTML <- sub('.*(<table class="grid".*?>.*</table>).*', '\\1', webHTML)
  
  # Parse the table
  tableList <- readHTMLTable(tableHTML)
  
  indexMembersTemp <- tableList$yfncsumtab
  
  if( is.null(indexMembersTemp))
  {
      return(NULL)
  }
  else
  {
    rowIndex <- which( indexMembersTemp$V1 =="Symbol")
    
    if( length(rowIndex)==0 | rowIndex == nrow(indexMembersTemp))
    {
      return(NULL)
    }
    else
    {
      colnames(indexMembersTemp) <- t(indexMembersTemp[rowIndex,])
      
      indexMembersTemp <- indexMembersTemp[ (rowIndex+1):nrow(indexMembersTemp), ]
      
      indexMembersTemp$Symbol <- as.character( indexMembersTemp$Symbol)
      
      indexMembersTemp$Symbol.Google <- sub(".L","",  indexMembersTemp$Symbol, fixed=T)
      
      indexMembersTemp$Name <- as.character( indexMembersTemp$Name)
      indexMembersTemp$'Last Trade' <- as.character( indexMembersTemp$'Last Trade')
      
      return(indexMembersTemp)
    }
    
  }

}



# stockIndex <- "FTSE"
# 
# indexMembers <- GetIndexMembers(stockIndex)
# 
# indexMembers$Symbol <- as.character(  indexMembers$Symbol)
# 
# 
# loadYahooData <- getSymbols(indexMembers$Symbol[2:6],src='yahoo')
# 
# #getSymbols(members$Symbol[2:6],src='google')
# 
# #plot(AAL.L  )
# stock <- "AAL.L"
# 
# plot(get(stock))
# 
# indexMoves <-  quantmod::getQuote(indexMembers$Symbol, src="yahoo")
# 
# View(indexMoves)

