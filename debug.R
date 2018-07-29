fx <- function(dataset){
  #diff function: next index value - now value
  #example:
  #> vectorColumn
  #[1] 10 10 10 10 10 15 13 14 12 13 14 10 12 14 15 15 11 14 15 10 14 12 15 15 12 11 15 10 14 10 15 14
  #> diff(vectorColumn)
  #[1]  0  0  0  0  5 -2  1 -2  1  1 -4  2  2  1  0 -4  3  1 -5  4 -2  3  0 -3 -1  4 -5  4 -4  5 -1
  
  i<-1
  
  vectorTotalGrowth <- c()
  vectorStartIndex <- c()
  vectorEndIndex <- c()
  
  #staatus = 0, no calculating
  status1 <- isNegative[1]
  status2 <- isNegative[1]
  tempGrowth <-0
  isNegative <- dataset<0
  
  
  vectorStartIndex[[1]] <- 1
  counter <- 1
  
  for(i in i:length(dataset)){
    print(dataset[i])
    #cat("s1:", status1, " s2:", status2,"\n")
    status1 <- status2
    status2 <- isNegative[i]
    if(i==length(dataset)){
      cat("tg", tempGrowth, "\n")
      cat("dt", dataset[i], "\n")
      # cat("dt", dataset[i-1], "\n")
      cat("ct", counter, "\n")
      print("last")
     
    }
    
    if(status1 == status2){
      
      print("sama")
      
      
      if(i==length(dataset)){
        tempGrowth <- tempGrowth + dataset[i]
        vectorTotalGrowth[[counter]] <- tempGrowth
      }else{
        tempGrowth <- tempGrowth + dataset[i]
      }
      vectorEndIndex[[counter]] <- i
    }else{
      
      print("beda")
      print(tempGrowth)
      vectorTotalGrowth[[counter]] <- tempGrowth
      
      counter<- counter+1
      
      vectorStartIndex[[counter]] <- i
      vectorEndIndex[[counter]] <- i
      
      if(i==length(dataset)){
        tempGrowth <- dataset[i]
        vectorTotalGrowth[[counter]] <- tempGrowth
      }
      tempGrowth <- 0
      
      tempGrowth <- dataset[i]
      status2 <- isNegative[i]
      
      
      #print(counter)
      
    }
    #print(dataset[i])
  }
  
  print(counter)
  print(dataset)
  cat("\n")
  print(vectorTotalGrowth)
  print(vectorStartIndex)
  print(vectorEndIndex)
  
  #return(listResult)
}

fx(asd)
