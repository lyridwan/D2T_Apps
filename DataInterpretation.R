
"Created By: Muhammad Ridwan
Date: 4th April 2018 8:17 AM
Data Interpret Function"
#-------------------------------  DATA INTERPRETATION -------------------------------

# AQValue <- AirQualityCalculation(airQualityDataset[1,]);


MembershipClassifier <- function(value, corpus){
  
  interpreterResult <- sapply(value, function(v) corpus[v >= corpus["Lower"] & v < corpus["Upper"],"Category"])
  interpreterIndex <- which(interpreterResult == corpus$Category)
  return (list(InterpreterResult = as.character(interpreterResult), InterpreterIndex = interpreterIndex))
}

#TO BE UPDATED!!!!
MembershipFuzzy <- function(value, corpus){
  if(is.null(corpus)){
    return (list(InterpreterResult = as.character("Constant"), InterpreterIndex = 0))
  }
  
  value <- as.double(value)
  
  i <- 1;
  n <- nrow(corpus);
  m <- length(corpus);
  
  membershipValue <- c()
  for(i in i:n){
    v1<-corpus[i, "v1"];
    v2<-corpus[i, "v2"];
    v3<-corpus[i, "v3"];
    v4<-corpus[i, "v4"];
    
    ##/ ¯ \ <- 1st area, 2nd area, 3rd area
    #first area
    if((value>=v1)&&(value<=v2)){
      membershipValue[i] <- (  (value-v1) / (v2-v1)  );
      #second area (optimum)
    }else if((value>v2)&&(value<=v3)){
      membershipValue[i] <- 1;
      #third area
    }else if((value>v3)&&(value<=v4)){
      membershipValue[i] <- (  (v4-value) / (v4-v3)  );
      #fourth, default condition (outside)
    }else{
      membershipValue[i] <- 0;
    }
    
    
    if(is.nan(as.numeric(membershipValue[i]))){
      membershipValue[i] <- 9999
    }
  }
  print(membershipValue)
  
  if(value < corpus[1, "v1"]){
    membershipValue[1] <- 9999
  }else if(value > corpus[length(membershipValue), "v4"]){
    membershipValue[length(membershipValue)] <- 9999
  }
  
  
  #check highest membership result
  interpreterResult <- corpus[which.max(membershipValue), "Category"]
  interpreterIndex <- which(interpreterResult == corpus$Category)
  return (list(InterpreterResult = as.character(interpreterResult), InterpreterIndex = interpreterIndex))
  # interval <- vector("list", n);
  # for (i in i:n) {
  #   interval[[i]] <- strsplit(as.character(corpus[i,"Interval"]), " ")[[1]];
  #   names(interval)[i] <- as.character(corpus[i, "Category"])
  # }
  # print(interval)
  # return(interval)
}

TrendFuzzyGenerator <-function(type, statisticalResume){
  corpus <- read.table(file=paste0("Corpus/Fuzzy/TrendFuzzyAdjective.csv"), sep=",", header=TRUE)
  maxRange <- as.character(statisticalResume[statisticalResume$ColName == type, "MaxValue"])
  minRange <- as.character(statisticalResume[statisticalResume$ColName == type, "MinValue"])
  
  #if minRange absolute value  more than maxRange
  if(abs(as.double(minRange)) > abs(as.double(maxRange))){
    temp <- abs(as.double(maxRange))
    maxRange <- abs(as.double(minRange))
    minRange <- temp
  }
  
  # corpus <- read.table(file=paste0("Corpus/GeneralAdjective.csv"), sep=",", header=TRUE)
  # maxRange <- as.character(statisticalResume[statisticalResume$ColName == "Tahu", "MaxValue"])
  # minRange <- as.character(statisticalResume[statisticalResume$ColName == "Tahu", "MinValue"])
  
  listGeneralPartition <- list()
  if(minRange == maxRange){
    result <- NULL
    return(result)
  }else{
    n = nrow(corpus)
    node = (2*n)+n-1
    
    minRange <- as.double(maxRange)/2 *-1
    maxRange <- as.double(maxRange)/2
    
    # cat(">>>> max min", maxRange, minRange)
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
        
        # cat(">>> i:", i, "<<<",v1,v2,v3,v4, "\n")
        listGeneralPartition[[i]] <- c(v1,v2,v3,v4)
        #listGeneralPartition[[corpus$Category[i]]] <- c(v1,v2,v3,v4)
      }else{
        
        v1<-minRange+(j)*rangenode;
        v2<-minRange+(j+1)*rangenode;
        v3<-minRange+(j+3)*rangenode;
        v4<-minRange+(j+4)*rangenode;
        # cat(">>> i:", i, "<<<",v1,v2,v3,v4, "\n")
        
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
  print(maxRange)
  print(listGeneralPartition[[1]][1])
  listGeneralPartition[[1]][1] <- as.double(maxRange)*-2
  listGeneralPartition[[length(listGeneralPartition)]][length(listGeneralPartition[[1]])] <- as.double(maxRange*2)
  
  print(listGeneralPartition[[1]][1])
  print(listGeneralPartition[[length(listGeneralPartition)]][length(listGeneralPartition[[1]])])
  
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
  PlottingTrendFuzzy(plotting, type)
  
  result <- data.frame(Category=corpus$Category, v1, v2, v3, v4)
  
  return(result)
}

GeneralFuzzyGenerator <-function(type, statisticalResume){
  corpus <- read.table(file=paste0("Corpus/Fuzzy/GeneralAdjective.csv"), sep=",", header=TRUE)
  maxRange <- as.character(statisticalResume[statisticalResume$ColName == type,"MaxValue"])
  minRange <- as.character(statisticalResume[statisticalResume$ColName == type,"MinValue"])
  
  # corpus <- read.table(file=paste0("Corpus/GeneralAdjective.csv"), sep=",", header=TRUE)
  # maxRange <- as.character(statisticalResume[statisticalResume$ColName == "Tahu", "MaxValue"])
  # minRange <- as.character(statisticalResume[statisticalResume$ColName == "Tahu", "MinValue"])
  
  listGeneralPartition <- list()
  if(minRange == maxRange){
    return(NULL)
  }else{
    n = nrow(corpus)
    node = (2*n)+n-1
    
    maxRange <- as.double(maxRange)
    minRange <- as.double(minRange)
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
        
        # cat(">>> i:", i, "<<<",v1,v2,v3,v4, "\n")
        listGeneralPartition[[i]] <- c(v1,v2,v3,v4)
        #listGeneralPartition[[corpus$Category[i]]] <- c(v1,v2,v3,v4)
      }else{
        # cat(">>> i:", i, "<<<",v1,v2,v3,v4, "\n")
        v1<-minRange+(j)*rangenode;
        v2<-minRange+(j+1)*rangenode;
        v3<-minRange+(j+3)*rangenode;
        v4<-minRange+(j+4)*rangenode;
        
        
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
  
  return(result)
}

#Plotting Fuzzy
#variables: list of 4 element of vector. Example: list(c(1,2,3,4))
PlottingFuzzy <- function(variables, name="Undefined"){
  matrix_graph <- list()
  y=as.matrix(c(1,6,6,1))
  n=length(variables)
  # print(n)
  maxX<-variables[[n]][4]
  minX<-variables[[1]][1]
  i=1;
  for(i in i:n){
    title <- paste(name," Membership Function")
    if(i==1){
      plot(variables[[i]],y,type="l",lwd=1,main=title,xlim=c(minX,maxX),yaxt="n",col="red")
    }else{
      lines(variables[[i]],y,lwd=1,col="red")
    }
  }
}

#Plotting Fuzzy
#variables: list of 4 element of vector. Example: list(c(1,2,3,4))
PlottingTrendFuzzy <- function(variables, name="Undefined"){
  matrix_graph <- list()
  
  n=length(variables)
  # print(n)
  maxX<-variables[[n]][4]
  i=1;
  for(i in i:n){
    y=as.matrix(c(1,6,6,1))
    title <- paste(name," Trend Membership Function")
    if(i==1){
      y[1,1] <-6
      plot(variables[[i]],y,type="l",lwd=1,main=title,xlim=c(maxX*-1,maxX),yaxt="n",col="red")
    }else if(i==n){
      y[4,1] <-6
      lines(variables[[i]],y,lwd=1,col="red")
    }else{
      lines(variables[[i]],y,lwd=1,col="red")
    }
  }
}

"
@Desc
--Interpretating value based on its corpus'
@param:
--value: value to check
--type: Type of Corpus (based on corpus folder) --TO BE UPDATED
@Example:
--Input: value=25, type='AirQuality'
--Output: Good
"
DataInterpreterAdjective <- function(value, type="General",statisticalResume=NULL){
  if(!is.na(mainConfig[mainConfig$ColName == type,]$Rule)){
    if(mainConfig[mainConfig$ColName == type,]$Rule == "fuzzy"){
      corpus <- read.table(file=paste0("Corpus/Fuzzy/",type,"Adjective.csv"), sep=",", header=TRUE)
      if(type == "Rainfall" && value == 0){
        result <- list(InterpreterResult = as.character("no rain"), InterpreterIndex = 0)
      }else{
        result <- MembershipFuzzy(value, corpus);
      }
    }else{
      corpus <- read.table(file=paste0("Corpus/Crisp/",type,"Adjective.csv"), sep=",", header=TRUE)
      result <- MembershipClassifier(value, corpus);
    }
  }else{
    corpus <- GeneralFuzzyGenerator(type, statisticalResume)
    result <- MembershipFuzzy(value, corpus);
  }
  
  # print(result)
  return(result)
}

AQDataInterpreterAdjective <- function(value, type="AirQuality"){
  
  AQparam <- c("CO","NO","NO2","NOX","O3","PM10","PM25","SO2")
  boolCase <- AQparam %in% mainConfig$ColName
  if(sum(boolCase == "FALSE") == 0){
    mainConfig <- rbind(mainConfig, c("AirQuality", "numeric", "range", NA))
  }
  
  if(!is.na(mainConfig[mainConfig$ColName == type,]$Rule)){
    if(mainConfig[mainConfig$ColName == type,]$Rule == "fuzzy"){
      corpus <- read.table(file=paste0("Corpus/Fuzzy/",type,"Adjective.csv"), sep=",", header=TRUE)
      if(type == "Rainfall" && value == 0){
        result <- list(InterpreterResult = as.character("no rain"), InterpreterIndex = 0)
      }else{
        result <- MembershipFuzzy(value, corpus);
      }
    }else{
      corpus <- read.table(file=paste0("Corpus/Crisp/",type,"Adjective.csv"), sep=",", header=TRUE)
      result <- MembershipClassifier(value, corpus);
    }
  }else{
    corpus <- GeneralFuzzyGenerator(type, statisticalResume)
    result <- MembershipFuzzy(value, corpus);
  }
  
  # print(result)
  return(result)
}

CorrelationInterpreterAdjective <- function(value){
  corpus <- read.table(file=paste0("Corpus/Crisp/CorrelationAdjective.csv"), sep=",", header=TRUE)
  result <- MembershipClassifier(value, corpus);
  
  return(result)
}

DataInterpreter <- function(dataset,statisticalResume){
  i <- 1;
  n <- length(dataset);
  
  #listInterpreterResult, return interpreter with their index, example - Interpreter: hot, Index: 3
  vectorColName <- c()
  vectorIntResult <- c()
  vectorIntIndex <- c()
  for(i in i:n){
    vectorColName[[i]] <- names(dataset[i])
    
    result <- DataInterpreterAdjective(dataset[i], type = names(dataset[i]), statisticalResume)
    #print(result)
    
    vectorIntResult[[i]] <- result$InterpreterResult
    vectorIntIndex[[i]] <- result$InterpreterIndex
    
    # print(DataInterpreterAdjective(dataset[i], type = names(dataset[i])))
  }
  
  #interpreterResult <- dataset;
  listResult <- list(Colname = vectorColName, InterpreterResult = vectorIntResult, InterpreterIndex = vectorIntIndex)
  #print(vectorIntResult)
  # print(listResult)
  return(listResult)
}


# interpreterResultYesterday <-DataIntepreter(datasetYesterday)
# interpreterResultToday <-DataIntepreter(datasetToday)
# interpreterResultLast2Month <- DataIntepreter(averageLast2Month)
# interpreterResultLastMonth <- DataIntepreter(averageLastMonth)
# interpreterResultYear <- DataIntepreter(averageYear)
# dummyCorpus <- read.table(file=paste0("Corpus/","Rainfall","Adjective.csv"), sep=",", header=TRUE)
# result <- MembershipFuzzy(10, dummyCorpus);
#result <- DataInterpreterAdjective(yesterdayDataset[1], type = names(yesterdayDataset[1]))

# cat("------------ Data Interpretation Output -----------\n\n")
# cat("------------ Yesterday -----------\n\n")
# for(i in 1:length(columnName)){
#   cat(" Yesterday ", columnName[i], ": ", as.character(unlist(interpreterResultYesterday[i])),"\n\n")
# }

# cat("------------ Data Interpretation Output -----------\n\n")
# cat("------------ Today -----------\n\n")
# for(i in 1:length(columnName)){
#   cat(" Today ", columnName[i], ": ", as.character(unlist(interpreterResultToday[i])),"\n\n")
# }

#Corpus
ResumeRepeated <- function (colName, dateTime, listRepeated){
  if(listRepeated$RepValue != 0){
    intro <- "There were some repeated values: "
    sentence <- ""
    
    phrase <- "stayed constant"
    sentence <- paste(sentence, colName, phrase, "on")
    
    timeRepeated <- ""
    j <- 1
    for(j in j:listRepeated$RepValue){
      indexStart <- listRepeated$Start[j]
      indexEnd <- listRepeated$End[j]
      
      #FORMAT: mm/dd/yyyy -> "07/01/2018"
      startMonth <- as.numeric(substr(dateTime[[indexStart]],1,2))
      endMonth <- as.numeric(substr(dateTime[[indexEnd]],1,2))
      
      startDate <- as.numeric(substr(dateTime[[indexStart]],4,5))
      endDate <- as.numeric(substr(dateTime[[indexEnd]],4,5))
      
      startYear <- as.numeric(substr(dateTime[[indexStart]],7,10))
      endYear <- as.numeric(substr(dateTime[[indexEnd]],7,10))
      
      if(startYear == endYear){
        if(startMonth == endMonth){
          #first index
          if(j==1){
            #Example: 1 - 8 June 2018
            timeRepeated <-paste0(startDate, "-", endDate, " ", month.abb[startMonth], " ", startYear)
            
            #!first index
          }else{
            #Example: 1 - 8 June 2018 , 1 - 8 Aug 2018
            timeRepeated <-paste0(timeRepeated,",", startDate, "-", endDate, " ", month.abb[startMonth], " ", startYear)
          }
          #if month different
        }else{
          #first index
          if(j == 1){
            #Example: 1 Mar - 8 June 2018
            timeRepeated <-paste(startDate, month.abb[startMonth], "-",  endDate, month.abb[endMonth], startYear)
            
            #!first index
          }else{
            #Example: 1 Mar - 8 June 2018
            timeRepeated <-paste(timeRepeated, ",", startDate, month.abb[startMonth], "-",  endDate, month.abb[endMonth], startYear)
          }
        }
      }else{
        #first index
        if(j == 1){
          #Example: 20 Jan 2017 - 08 Jan 2018
          timeRepeated <-paste(startDate, month.abb[startMonth], startYear, "-", endDate, month.abb[endMonth], endYear)
        }else{
          #Example: 20 Jan 2017 - 08 Jan 2018, 20 Feb 2017 - 08 Feb 2018
          timeRepeated <-paste(timeRepeated, startDate, month.abb[startMonth], startYear, "-", endDate, month.abb[endMonth], endYear)
        }
      }
      
    }#end for j
    sentence <- paste(sentence, timeRepeated,";")
  }#end if repValue
  
  return(sentence)
}#end function

ResumeRepeated2 <- function (colName, dataset, interpreterResult, vectorStart, vectorEnd){
  # print(interpreterResult)
  #During 6-21 Jul 2016, 13-20 Mar 2017, 2-24 Apr 2017, 14-21 Jun 2017 Rainfall stayed constant at 0 (no rain)
  if(length(interpreterResult) == 1){
    dateStart <- dataset[["DateTime"]][vectorStart[1]]
    dateEnd <- dataset[["DateTime"]][vectorEnd[1]]
    
    dateRange <- LexicalDateRange(dateStart,dateEnd)
    
    subSentence <- paste(colName, "stayed constant at", interpreterResult[1])
    subSentence <- paste0(subSentence, 
                          # " (", dataset[[colName]][vectorStart[i]],  " point)",
                          " during ", dateRange)
    mainSentence <- paste0(subSentence,".")
  }else if(length(interpreterResult) != 0){
    mainSentence<-""
    
    i<-1
    reps<- rep(0, length(interpreterResult))
    while(i < length(interpreterResult) && length(reps[reps == 0]) != 0 ){
      
      subSentence <- ""
      if(reps[i] == 0){
        reps[i] <- i
        
        dateStart <- dataset[["DateTime"]][vectorStart[i]]
        dateEnd <- dataset[["DateTime"]][vectorEnd[i]]
        
        dateRange <- LexicalDateRange(dateStart,dateEnd)
        subSentence <- paste0(dateRange)
        if(i != length(interpreterResult)){
          j <- i + 1
          while(j <= length(interpreterResult)){
            if(length(reps[reps == 0]) != 0){
              if(interpreterResult[i] == interpreterResult[j]){
                reps[j] <- j
                
                dateStart <- dataset[["DateTime"]][vectorStart[j]]
                dateEnd <- dataset[["DateTime"]][vectorEnd[j]]
                
                dateRange <- LexicalDateRange(dateStart,dateEnd)
                subSentence <- paste0(subSentence,", ", dateRange)
              }
              j <- j + 1
            }else{
              break
            }
          }
        }
      }
      
      # cat(reps, "\n")
      
      subSentence <- paste0(colName, " stayed constant at ", interpreterResult[i], 
                            # " (", dataset[[colName]][vectorStart[i]],  " point) ",
                            " during ", subSentence)
      mainSentence <- paste0(mainSentence, subSentence,".")
      i <- i + 1
    }
    
  }
  
  return(mainSentence)
}#end function

#------------------------------------------------------------------------------------------------------------------
# CURERNT ANALYSIS + CORPUS
#------------------------------------------------------------------------------------------------------------------

CurrentDesc <- function(interpreterResult, vectorTrendDesc, dataset){
  result <- ""
  
  #error handling
  if(length(vectorTrendDesc) != 0){
    mainSentence<-""
    
    #marking if the index has been grouped
    #example: 1 2 0 4 5 0 0
    # 0 represent that index still not been checked
    i<-1
    reps<- rep(0, length(vectorTrendDesc))
    
    #first itter
    while(i <= length(vectorTrendDesc) && length(reps[reps == 0]) != 0 ){
      
      subSentence <- ""
      if(reps[i] == 0){
        reps[i] <- i
        
        colName <- interpreterResult$Colname[i]
        
        subSentence <- paste0(colName)
        
        #handling if not the last value
        if(i != length(vectorTrendDesc)){
          j <- i + 1
          
          #second itter
          while(j <= length(vectorTrendDesc)){
            if(length(reps[reps == 0]) != 0){
              if(interpreterResult$InterpreterResult[i] == interpreterResult$InterpreterResult[j]){
                #mark the index as checked
                reps[j] <- j
                
                colName <- interpreterResult$Colname[j]
                
                #isGroupingAvailable: Vector, TRUE OR FALSE
                #TRUE FALSE
                #0    0
                indicator <- c("TRUE"=0, "FALSE"=0)
                isGroupingAvailable <- (interpreterResult$InterpreterResult[j:length(interpreterResult$InterpreterResult)] == interpreterResult$InterpreterResult[i])
                isGroupingAvailable <- table(isGroupingAvailable)
                
                #handling if table dont have FALSE or TRUE value, then use default value (0)
                #Example:
                #TRUE FALSE     TRUE FALSE     TRUE FALSE     TRUE FALSE
                #0    2         3    2         1    0         0    0
                if(!is.na(isGroupingAvailable["FALSE"]) && !is.na(isGroupingAvailable["TRUE"])){
                  indicator["FALSE"] <- as.numeric(isGroupingAvailable["FALSE"])
                  indicator["TRUE"] <- as.numeric(isGroupingAvailable["TRUE"])
                }else if(!is.na(isGroupingAvailable["TRUE"])){
                  indicator["TRUE"] <- as.numeric(isGroupingAvailable["TRUE"])
                }else if(!is.na(isGroupingAvailable["FALSE"])){
                  indicator["FALSE"] <- as.numeric(isGroupingAvailable["FALSE"])
                }
                
                # print((interpreterResult$InterpreterResult[j:length(interpreterResult$InterpreterResult)] == interpreterResult$InterpreterResult[i]) & vectorTrendDesc[j:length(vectorTrendDesc)] == vectorTrendDesc[i])
                # print(indicator)
                #if there's nothing to grouping, then put "and" on the end of grouping
                if(as.numeric(indicator["TRUE"]) > 1){
                  subSentence <- paste0(subSentence,", ", colName)
                }else{
                  subSentence <- paste0(subSentence,", and ", colName)
                }
                
                
              }
              j <- j + 1
            }else{
              break
            }
          }
        }
        
        phrase<-""
        #after groping the column
        interpreter <- interpreterResult$InterpreterResult[i]
        if(interpreter == "Constant"){
          interpreter <- change_word_bank_AQ2() 
          phrase <- "from the first time"
        }else{
          phrase <- "condition"
        }
        
        subSentence <- paste(subSentence, "in", interpreter, phrase)
        mainSentence <- paste0(mainSentence, subSentence,". ")
      }
      # cat("reps:", i, " ",reps,"\n")
      
      i <- i + 1
    }
    
  }#end if repValue
  
  return(mainSentence)
}

MotifDiscoveryInterpreter <- function(dataset, datasetIntervalValue){
  result <- NA
  
  
  listMD <- list()
  i<-1
  for(i in i:length(dataset)){
    print(colnames(dataset)[i])
    listMD[[i]] <- MotifDiscoveryAnalysis(colnames(dataset)[i], dataset[[i]], datasetIntervalValue)
  }
  return(listMD)
}

InterpreterExtremeEvent <- function (value, statisticalResume){
  #Initalizing
  message <- NULL
  minVal <- as.numeric(as.character(statisticalResume$MinValue))
  maxVal <- as.numeric(as.character(statisticalResume$MaxValue))
  
  rangeVal <- maxVal - minVal
  
  print(value)
  print(0.75*rangeVal)
  #if value is higher then 65% range data its mean Extreme Event
  if(abs(value) > (0.65*rangeVal)){
    message <- "extreme"
  }else{
    message <- "normal"
  }
  
  return(message)
}

ComparsionMessage <- function(data, result, interval){
  limitInterpreter <- DataInterpreterInterval(interval, type = "lastinterpreter")
  
  groupedParam1 <- c()
  if(sum(result == "equal with") == 1){
    index <- which(result == "equal with")
    
    groupedParam1[1] <- names(data[index])
  }else if(sum(result == "equal with") > 1){
    i<-1
    index <- 1
    for(i in i:length(result)){
      if(result[i] == "equalwith"){
        groupedParam1[index] <- names
        index <- index + 1
      }
    }
  }
  
  groupedParam2 <- c()
  if(sum(result == "higher than") == 1){
    index <- which(result == "higher than")
    
    groupedParam2[1] <- names(data[index])
  }else if(sum(result == "higher than") > 1){
    i<-1
    index <- 1
    for(i in i:length(result)){
      if(result[i] == "higher than"){
        groupedParam2[index] <- names(data[i])
        index <- index + 1
      }
    }
  }
  
  groupedParam3 <- c()
  if(sum(result == "lower than") == 1){
    index <- which(result == "lower than")
    
    groupedParam3[1] <- names(data[index])
  }else if(sum(result == "lower than") > 1){
    i<-1
    index <- 1
    for(i in i:length(result)){
      if(result[i] == "lower than"){
        groupedParam3[index] <- names(data[i])
        index <- index + 1
      }
    }
  }
  
  groupedMsg1 <- ComparsionAggregation(groupedParam1)
  groupedMsg2 <- ComparsionAggregation(groupedParam2)
  groupedMsg3 <- ComparsionAggregation(groupedParam3)
  
  sentence <- ""
  if(!is.null(groupedParam1)){
    if(length(groupedParam1) == 1){
      tobe <- "is"
      s <- ""
    }else{
      tobe <- "are"
      s <- "s"
    }
    sentence<- paste0(groupedMsg1, " parameter", s, " ", tobe, " equal with ", limitInterpreter, "'s data. ")
  }
  
  #IF ALL LOWER THAN
  if(is.null(groupedMsg2)){
    if(length(groupedParam3) == 1){
      tobe <- "is"
      s <- ""
    }else if(length(groupedParam3) == length(data)){
      groupedMsg2 <- "All"
    }else{
      tobe <- "are"
      s <- "s"
    }
    
    sentence<- paste0(sentence, groupedMsg3, " parameter", s, " ", tobe, " lower than ", limitInterpreter, "'s data.")
    
    #IF ALL HIGHER THAN
  }else if(is.null(groupedMsg3)){
    if(length(groupedParam2) == 1){
      tobe <- "is"
      s <- ""
    }else if(length(groupedParam2) == length(data)){
      groupedMsg2 <- "All"
      tobe <- "are"
      s <- "s"
    }else{
      tobe <- "are"
      s <- "s"
    }
    
    
    sentence<- paste0(sentence, groupedMsg2, " parameter", s, " ", tobe, " higher than ", limitInterpreter, "'s data.")
  }else{
    if(length(groupedParam2) <= length(groupedParam3)){
      if(length(groupedParam2) == 1){
        tobe <- "is"
        s <- ""
      }else{
        tobe <- "are"
        s <- "s"
      }
      
      sentence<- paste0(sentence, groupedMsg2, " parameter", s, " ", tobe, " higher than ", limitInterpreter, "'s data, ",
                        "but the rest parameters are lower than ", limitInterpreter, "'s data.")
    }else{
      if(length(groupedParam3) == 1){
        tobe <- "is"
        s <- ""
      }else{
        tobe <- "are"
        s <- "s"
      }
      
      sentence<- paste0(sentence, groupedMsg3, " parameter", s, " ", tobe, " lower than ", limitInterpreter, "'s data, ",
                        "but the rest parameters are higher than ", limitInterpreter, "'s data.")
    }
  }
  
  print(sentence)
  
}

CorrelationSignificantMessage <- function(corMatrix){
  #Correlation Significant Message Analysis
  
  corMatrix<-CorrelationSignificantMsgContentDetermination(corMatrix)
  # Example: 
  
  # PM2.5       DEWP       TEMP PRES LWS IS IR
  # PM2.5     0  0.0000000  0.0000000    0   0  0  0
  # DEWP      0  0.0000000  0.0000000    0   0  0  0
  # TEMP      0  0.8580855  0.0000000    0   0  0  0
  # PRES      0 -0.7387440 -0.7730187    0   0  0  0
  # LWS       0  0.0000000  0.0000000    0   0  0  0
  # IS        0  0.0000000  0.0000000    0   0  0  0
  # IR        0  0.0000000  0.0000000    0   0  0  0
  
  #IF THERE's NO SIGNIFICANT MESSAGE (*doesn't have strong impact)
  if(sum(corMatrix != 0) == 0){
    return(NULL)
  }
  
  # Decide, we read as a row, or column
  # The higher val win
  rowMode <- max(abs(apply(corMatrix, 1, sum)))
  colMode <- max(abs(apply(corMatrix, 2, sum)))
  
  result<-""
  
  # If reading as a row
  if(rowMode > colMode){
    # Get the row index when value != 0
    vecParam <- which(apply(corMatrix, 1, sum) != 0)
    # TEMP PRES 
    # 3    4
    
    
    i<-1
    for(i in i:length(vecParam)){
      xParam <- names(vecParam)[i]
      # [1] "TEMP"
      
      yParam <- c("")
      yValue <- c("")
      
      j<-1
      index <- 1
      for(j in j:length(corMatrix)){
        if(corMatrix[xParam,j] != 0){
          yParam[index] <- names(corMatrix)[j]
          
          #Checking Trend
          msgTrend <- statisticalResume[statisticalResume$ColName == xParam, "Trend"]
          #If Relationship is Positive 
          if(corMatrix[xParam,j] >= 0){
            if(msgTrend == "+"){
              yValue[index] <- "posInc"
            }else{
              yValue[index] <- "posDec"
            }
            
            #If Relationship is Negative 
          }else{
            if(msgTrend == "+"){
              yValue[index] <- "negInc"
            }else{
              yValue[index] <- "negDec"
            }
          }
          
          #Increment
          index <- index + 1
        }
      }
      
      # xParam 
      # [1] "TEMP"
      # yParam 
      # [1] "DEWP"
      # yValue
      # [1] "posInc"
      sentence <- CorrelationSignificantMsgAggregation(xParam, yParam, yValue)
      # result
      # "A rise in TEMP causes an attendant increase in DEWP."
      # explanation: Increase in TEMP causing (Temp trend is +) causing positive relation (0.8580855) to DEWP
      # NOTE: sentence, can be multiple EXAMPLE: "  An increase in DEWP resulted an increase in TEMP. 
      #                                             An increase in DEWP resulted a decay in PRES."
      
      result <- paste(result, sentence)
    } 
  }else{
    vecParam <- which(apply(corMatrix, 2, sum) != 0)
    
    i<-1
    for(i in i:length(vecParam)){
      xParam <- names(vecParam)[i]
      yParam <- c("")
      yValue <- c("")
      
      j<-1
      index <- 1
      for(j in j:length(corMatrix)){
        if(corMatrix[j,xParam] != 0){
          yParam[index] <- names(corMatrix)[j]
          
          msgTrend <- statisticalResume[statisticalResume$ColName == xParam, "Trend"]
          if(corMatrix[j,xParam] >= 0){
            if(msgTrend == "+"){
              yValue[index] <- "posInc"
            }else{
              yValue[index] <- "posDec"
            }
          }else{
            if(msgTrend == "+"){
              yValue[index] <- "negInc"
            }else{
              yValue[index] <- "negDec"
            }
          }
          index <- index + 1
        }
      }
      sentence <- CorrelationSignificantMsgAggregation(xParam, yParam, yValue)
      
      result <- paste(result, sentence)
    }
  }
  
  result <- gsub(".  ", ". ", result)
  result <- gsub("  ", "", result)
  return(result)
}

CorrelationRoutineMessage <- function(corMatrix){
  #Correlation Routine Message Analysis
  #Mean with absolute value
  coreMean <- apply(abs(corMatrix), 2, mean)
  
  #get the highest mean with their index
  highestMean <- max(coreMean)
  highestIndex <- which.max(coreMean)
  
  message <- correlationRoutineDocPlan(highestMean, names(highestIndex))
  return(message)
}

