TrendFuzzyGenerator <-function(type, statisticalResume){
  corpus <- read.table(file=paste0("Corpus/TrendFuzzyAdjective.csv"), sep=",", header=TRUE)
  maxRange <- as.character(statisticalResume[statisticalResume$ColName == type, "MaxValue"])
  minRange <- as.character(statisticalResume[statisticalResume$ColName == type, "MinValue"])
  
  # corpus <- read.table(file=paste0("Corpus/GeneralAdjective.csv"), sep=",", header=TRUE)
  # maxRange <- as.character(statisticalResume[statisticalResume$ColName == "Tahu", "MaxValue"])
  # minRange <- as.character(statisticalResume[statisticalResume$ColName == "Tahu", "MinValue"])
  
  listGeneralPartition <- list()
  if(minRange == maxRange){
    result <- "Constant"
  }else{
    n = nrow(corpus)
    node = (2*n)+n-1
    
    minRange <- as.double(maxRange)/2*-1
    maxRange <- as.double(maxRange)/2
    
    cat(">>>> max min", maxRange, minRange)
    rangenode = (maxRange-minRange)/node
    
    i=1
    j=0
    membershipValue <- c()
    for (i in i:n) {
      if(i == 1){
        v1<-minRange;
        v2<-minRange;
        v3<-minRange+(2*rangenode);
        v4<-minRange+(3*rangenode);
        
        # print("````````````````````````````")
        # print(j)
        j <- i+1
        # print(j)
        # print(value)
        # print(membershipValue[i])
        # print(v1)
        # print(v2)
        # print(v3)
        # print(v4)
        
        cat(">>> i:", i, "<<<",v1,v2,v3,v4, "\n")
        listGeneralPartition[[i]] <- c(v1,v2,v3,v4)
        #listGeneralPartition[[corpus$Category[i]]] <- c(v1,v2,v3,v4)
      }else{
        
        v1<-minRange+(j)*rangenode;
        v2<-minRange+(j+1)*rangenode;
        v3<-minRange+(j+3)*rangenode;
        v4<-minRange+(j+4)*rangenode;
        cat(">>> i:", i, "<<<",v1,v2,v3,v4, "\n")
        
        listGeneralPartition[[i]] <- c(v1,v2,v3,v4)
        
        #lines(c(v1,v2,v3,v4),as.matrix(c(1,6,6,1)),lwd=1,col="red")
        # print("````````````````````````````")
        # print(j)
        j <- j+3
        # print(j)
        # print(value)
        # print(membershipValue[i])
        # print(v1)
        # print(v2)
        # print(v3)
        # print(v4)
      }
    }
    
    # result <- corpus[2, "Category"]
    # result<- paste("Not available for general -aa",which.max(membershipValue))
    ##corpus <- read.table(file="Corpus/GeneralAdjective.csv", sep=",", header=TRUE)
    ##result <- MembershipFuzzy(value, corpus);
  }
  
  #exception for first and last point
  listGeneralPartition[[1]][1] <- as.double(as.character(statisticalResume[statisticalResume$ColName == type,"MaxValue"]))*-1
  listGeneralPartition[[length(listGeneralPartition)]][length(listGeneralPartition[[1]])] <- as.double(as.character(statisticalResume[statisticalResume$ColName == type,"MaxValue"]))
  
  
  v1 <-unlist(lapply(listGeneralPartition, `[[`, 1))
  v2 <-unlist(lapply(listGeneralPartition, `[[`, 2))
  v3 <-unlist(lapply(listGeneralPartition, `[[`, 3))
  v4 <-unlist(lapply(listGeneralPartition, `[[`, 4))
  
  #PLOTTING AREA
  i<-1
  plotting <- list()
  for(i in i:length(listGeneralPartition)){
    plotting[[i]] <- listGeneralPartition[[i]]
  }
  PlottingFuzzy(plotting, type)
  
  result <- data.frame(Category=corpus$Category, v1, v2, v3, v4)
  
  return(listGeneralPartition)
}

#Plotting Fuzzy
#variables: list of 4 element of vector. Example: list(c(1,2,3,4))
PlottingFuzzy <- function(variables, name="Undefined"){
  matrix_graph <- list()
  
  n=length(variables)
  # print(n)
  maxX<-variables[[n]][4]
  i=1;
  for(i in i:n){
    y=as.matrix(c(1,6,6,1))
    title <- paste(name," Membership Function")
    if(i==1){
      y[1,1] <-6
      plot(variables[[i]],y,type="l",lwd=1,main=title,xlim=c(maxX,maxX),yaxt="n",col="red")
    }else if(i==n){
      y[4,1] <-6
      lines(variables[[i]],y,lwd=1,col="red")
    }else{
      lines(variables[[i]],y,lwd=1,col="red")
    }
  }
}



TrendFuzzyGenerator("Temperature", statisticalResume)

#_____________________________________________________________________________________________

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
      print("last")
      if(status1 == status2){
        tempGrowth <- tempGrowth + dataset[i]
      }else{
        tempGrowth <- 0
      }
      vectorTotalGrowth[[counter]] <- tempGrowth
    }
    
    if(status1 == status2){
      print("sama")
      tempGrowth <- tempGrowth + dataset[i]
      vectorEndIndex[[counter]] <- i
    }else{
      print("beda")
      print(tempGrowth)
      vectorTotalGrowth[[counter]] <- tempGrowth
      
      counter<- counter+1
      
      
      vectorStartIndex[[counter]] <- i
      vectorEndIndex[[counter]] <- i
      
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
