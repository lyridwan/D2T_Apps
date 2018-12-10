StatisticalAnalysis <- function(dataset){
  datasetWithoutDate <- dataset[ , colnames(dataset) != "DateTime"]
  ColName <- SumValue <- Average <- MaxValue <- MaxIndex <- MaxDate <- MinValue <- MinIndex <- MinDate <- c("")
  
  i=1
  n=length(datasetWithoutDate)
  for(i in i:n){
    ColName[i] <- colnames(datasetWithoutDate[i])
    
    #MAX
    MaxValue[i] <- max(datasetWithoutDate[i])
    max_index2 <- as.integer(which(datasetWithoutDate[i]==max(datasetWithoutDate[i])))
    MaxIndex[i] <- max_index2[1]
    max_index0 <- max_index2[1]
    MaxDate[i] <- as.character(dataset$Date[max_index0])
    
    #MIN
    MinValue[i] <- min(datasetWithoutDate[i])
    min_index2 <- as.integer(which(datasetWithoutDate[i]==min(datasetWithoutDate[i])))
    MinIndex[i] <- min_index2[1]
    min_index0 <- min_index2[1]
    MinDate[i] <- as.character(dataset$Date[min_index0])
    
    #SUM
    SumValue[i] <- sum(datasetWithoutDate[,i])
    
    #AVERAGE
    Average[i] <- mean(datasetWithoutDate[,i])
  }
  
  datasetStatistical <- data.frame(ColName, MaxDate, MaxValue, MaxIndex, MinDate, MinValue, MinIndex, SumValue, Average);
  return(datasetStatistical) 
}

PredictDataset<-function(dataset, format="%m/%d/%Y %H:%S"){
  result <- c()
  lengthWithoutDate <-  length(dataset[,-which(colnames(dataset) == "DateTime")])
  
  dataSeries <- xts(dataset[,-which(colnames(dataset) == "DateTime")], order.by=strptime(dataset[,"DateTime"], format))
  print(head(dataSeries))
  i<-1
  for(i in i:lengthWithoutDate){
    if(length(unique(dataSeries[,i])) != 1){
      print("NAS")
      result[i] <- forecast(dataSeries[,i], h=1)$mean
    }else{
      
      result[i] <- dataSeries[1,i]
    }
    
  }
  
  names(result) <- colnames(dataset[ , colnames(dataset) != "DateTime"])
  
  return(result)
}

AirQualityCalculation <- function (dataset){
  #menghitung sub-index value dari variabel PM25
  #b2 = polutant concentration breakpoint 2, a2 = PSI Index breakpoint 2
  #b1 = polutant concentration breakpoint 1, a1 = PSI Index breakpoint 1
  a2<-0;a1<-0;b2<-0;b1<-0;PM25_PSI_value<-0;
  PM10_PSI_value<-0; CO_PSI_value=0;
  NO2_PSI_value<-0; SO2_PSI_value <-0;
  O3_PSI_value <- 0;
  #define each sub-index interval of PM25
  PM25<-as.double(dataset["PM25"])
  if((PM25>=0)&&(PM25<=12)){
    b2<-12; b1<-0; a2<-50; a1<-0;
  }
  else if((PM25>12)&&(PM25<=55)){
    b2<-55; b1<-13; a2<-100; a1<-51
  }
  else if((PM25>55)&&(PM25<=150)){
    b2<-150; b1<-56; a2<-200; a1<-101
  }
  else if((PM25>150)&&(PM25<=250)){
    b2<-250; b1<-151; a2<-300; a1<-201
  }
  else if((PM25>250)&&(PM25<=350)){
    b2<-350; b1<-251; a2<-400; a1<-301
  }
  else if((PM25>350)&&(PM25<=500)){
    b2<-500; b1<-351; a2<-500; a1<-401
  }
  else{
    b2<-500; b1<-351; a2<-500; a1<-401
  }
  PM25_PSI_value <- ((a2-a1)/(b2-b1))*(PM25-b1)+a1
  
  a2<-0;a1<-0;b2<-0;b1<-0;
  #define each sub-index interval of PM10
  PM10<-as.double(dataset["PM10"])
  if((PM10>=0)&&(PM10<=50)){
    b2<-50; b1<-0; a2<-50; a1<-0;
  }
  else if((PM10>=51)&&(PM10<=150)){
    b2<-150; b1<-51; a2<-100; a1<-51
  }
  else if((PM10>=151)&&(PM10<=350)){
    b2<-350; b1<-151; a2<-200; a1<-101
  }
  else if((PM10>=351)&&(PM10<=420)){
    b2<-420; b1<-351; a2<-300; a1<-201
  }
  else if((PM10>=421)&&(PM10<=500)){
    b2<-500; b1<-421; a2<-400; a1<-301
  }
  else if((PM10>=501)&&(PM10<=600)){
    b2<-600; b1<-501; a2<-500; a1<-401
  }
  else{
    b2<-600; b1<-501; a2<-500; a1<-401	
  }
  PM10_PSI_value <- ((a2-a1)/(b2-b1))*(PM10-b1)+a1
  
  a2<-0;a1<-0;b2<-0;b1<-0;
  SO2<-as.double(dataset["SO2"])
  if((SO2>=0)&&(SO2<=12)){
    b2<-12; b1<-0; a2<-50; a1<-0;
  }
  else if((SO2>12)&&(SO2<=55)){
    b2<-55; b1<-13; a2<-100; a1<-51
  }
  else if((SO2>55)&&(SO2<=150)){
    b2<-150; b1<-56; a2<-200; a1<-101
  }
  else if((SO2>150)&&(SO2<=250)){
    b2<-250; b1<-151; a2<-300; a1<-201
  }
  else if((SO2>250)&&(SO2<=350)){
    b2<-350; b1<-251; a2<-400; a1<-301
  }
  else if((SO2>350)&&(SO2<=500)){
    b2<-500; b1<-351; a2<-500; a1<-401
  }
  else{
    b2<-500; b1<-351; a2<-500; a1<-401
  }
  SO2_PSI_value <- ((a2-a1)/(b2-b1))*(SO2-b1)+a1
  
  a2<-0;a1<-0;b2<-0;b1<-0;
  CO<-as.double(dataset["CO"])
  if((CO>=0)&&(CO<=5)){
    b2<-5; b1<-0; a2<-50; a1<-0;
    # print("xx11")
  }
  else if((CO>5)&&(CO<=10)){
    b2<-10; b1<-5.1; a2<-100; a1<-51
    # print("xx12")
  }
  else if((CO>10)&&(CO<=17)){
    b2<-17; b1<-10; a2<-200; a1<-101
    # print("xx13")
  }
  else if((CO>17)&&(CO<=34)){
    b2<-34; b1<-17.1; a2<-300; a1<-201
    # print("xx14")
  }
  else if((CO>34)&&(CO<=46)){
    b2<-46; b1<-34.1; a2<-400; a1<-301
    # print("xx15")
  }
  else if((CO>46)&&(CO<=57.5)){
    b2<-57.5; b1<-46.1; a2<-500; a1<-401
    # print("xx16")
  }
  else{
    b2<-57.5; b1<-46.1; a2<-500; a1<-401
  }
  xy=0;
  CO_PSI_value <- (((a2-a1)/(b2-b1))*(CO-b1)+a1)
  
  
  a2<-0;a1<-0;b2<-0;b1<-0;
  O3<-as.double(dataset["O3"])
  if((O3>=0)&&(O3<=118)){
    b2<-118; b1<-0; a2<-50; a1<-0;
  }
  else if((O3>=119)&&(O3<=157)){
    b2<-157; b1<-119; a2<-100; a1<-51
  }
  else if((O3>=158)&&(O3<=235)){
    b2<-235; b1<-158; a2<-200; a1<-101
  }
  else if((O3>=236)&&(O3<=785)){
    b2<-785; b1<-236; a2<-300; a1<-201
  }
  else if((O3>=786)&&(O3<=980)){
    b2<-980; b1<-786; a2<-400; a1<-301
  }
  else if((O3>=981)&&(O3<=1180)){
    b2<-1180; b1<-981; a2<-500; a1<-401
  }
  else{
    b2<-1180; b1<-981; a2<-500; a1<-401
  }
  O3_PSI_value <- (((a2-a1)/(b2-b1))*(O3-b1)+a1)
  
  
  PSI_data <- c(PM25_PSI_value,PM10_PSI_value,SO2_PSI_value,CO_PSI_value,O3_PSI_value)
  PSI_value <- as.integer(max(PSI_data))
  return(PSI_value);
}

#Repeated Value analysis
ResumeRepeatedAnalysis <- function(dataset){
  
  lengthEncoding <- rle(dataset)
  #Example:
  #Run Length Encoding
  #lengths: int [1:26] 5 1 1 1 1 1 1 1 1 1 ...
  #values : int [1:26] 10 15 13 14 12 13 14 10 12 14 ...
  
  #limit
  # n <- DataInterpreterInterval(datasetIntervalValue, type = "limit")
  n <- length(dataset) * 0.1
  
  repeatedSequence <- rep(lengthEncoding$lengths >= n, times=lengthEncoding$lengths)
  #Example repeatedSequence
  #[1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
  #[20]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
  
  #print(repeatedSequence)
  
  RepValue <- as.numeric(table(repeatedSequence)["TRUE"])
  if(!is.na(RepValue)){
    # Get 
    dt <- data.frame(number = rle(repeatedSequence)$values, lengths = rle(repeatedSequence)$lengths)
    # Get the end
    dt$end <- cumsum(dt$lengths)
    # Get the start
    dt$start <- dt$end - dt$lengths + 1
    # Selecting column
    dt <- dt[dt$number == TRUE, c("number", "start", "end")]
    result <- list(RepValue = nrow(dt), Start = dt$start, End = dt$end)
  }else{
    result <- list(RepValue = 0, Start = 0, End = 0)
  }
  
  return(result)
}

ResumeHighestGrowthAnalysis <- function(dataset, type=NULL){
  #Count up all positive and negative value
  #With their index
  #EXAMPLE:
  #dataset:  1  2 -2  1 -1  2  2
  #result :  3 -2  1 -1  4
  #Start  : 1 3 4 5 6
  #End    : 2 3 4 5 7
  i<-1
  
  vectorTotalGrowth <- c()
  vectorStartIndex <- c()
  vectorEndIndex <- c()
  
  #isNegative: Vector, true if negative, false if positive
  isNegative <- dataset
  isNegative[isNegative==0] <- NA
  
  isNegative <- isNegative<0
  
  # print(isNegative)
  status1 <- TRUE
  status2 <- TRUE
  tempGrowth <-0
  
  
  vectorStartIndex[[1]] <- 1
  counter <- 1
  
  #Main Calculation
  for(i in i:length(dataset)){
    if(!is.na(isNegative[i])){
      #update param value while itterate
      status1 <- status2
      status2 <- isNegative[i]
      
      #if type is same, ex: positive-positive, negative-negative
      if(status1 == status2){
        #for last dataset
        if(i==length(dataset)){
          tempGrowth <- tempGrowth + dataset[i]
          vectorTotalGrowth[[counter]] <- tempGrowth
        }else{
          tempGrowth <- tempGrowth + dataset[i]
        }
        vectorEndIndex[[counter]] <- i
        
        #if n != n-1, positive-negative
      }else{
        #storing variable, counter++
        vectorTotalGrowth[[counter]] <- tempGrowth
        
        counter<- counter+1
        
        vectorStartIndex[[counter]] <- i
        vectorEndIndex[[counter]] <- i
        
        #for last dataset
        if(i==length(dataset)){
          tempGrowth <- dataset[i]
          vectorTotalGrowth[[counter]] <- tempGrowth
        }
        
        #resetting parameter
        tempGrowth <- 0
        
        tempGrowth <- dataset[i]
        status2 <- isNegative[i]
        
      }
    }else{
      status1 <- "SKIP"
      status2 <- "SKIP"
      
      vectorTotalGrowth[[counter]] <- tempGrowth
      vectorEndIndex[[counter]] <- i
    }
  }
  
  #Return highest growth, with their start-end index
  #Default, Returning all analysis result as big list  
  if(is.null(type)){
    return(list(TotalGrowth=vectorTotalGrowth, StartIndex=vectorStartIndex, EndIndex=vectorEndIndex))
  }else if(type == "Growth"){
    indexResult <- which.max(vectorTotalGrowth)
    valueResult <- vectorTotalGrowth[indexResult]
    
    startIndexResult <- vectorStartIndex[indexResult]
    endIndexResult <- vectorEndIndex[indexResult]
    
    return(list(valueResult=valueResult, startIndexResult=startIndexResult, endIndexResult=endIndexResult))
    
    #Return lowest decay, with their start-end index
  }else if (type == "Decay"){
    indexResult <- which.min(vectorTotalGrowth)
    valueResult <- vectorTotalGrowth[indexResult]
    
    startIndexResult <- vectorStartIndex[indexResult]
    endIndexResult <- vectorEndIndex[indexResult]
    
    return(list(valueResult=valueResult, startIndexResult=startIndexResult, endIndexResult=endIndexResult))
  }
}

TrendAnalysis <- function(start,dataset, min, max){
  # Dataset is vector, pokonamah ka gigir
  plot(as.numeric(unlist(dataset)), type="o", col="blue")
  dataset <- dataset[start:length(dataset)]
  
  if(length(unique(dataset)) == 1){
    result <- "0"
  }else{
    x = c(1:length(dataset))  
    reg = lm(dataset~x)
    
    #linear model range
    df <- reg$coefficients[2] + reg$coefficients[1]
    dl <- reg$coefficients[2]*length(dataset) + reg$coefficients[1]
    range <- dl-df
    
    print(df)
    print(dl)
    print(range)
    
    
    #stats
    stat <- 0
    if(range < 0){
      range <- range * (-1)
      stat <- 1
    }
    
    #dataset range
    rangeReal <- max-min
    
    print("----")
    print(min)
    print(max)
    print(rangeReal)
    print(0.05*rangeReal)
    #5% minimum tershold
    if(range > 0.05*rangeReal){
      if(stat == 0){
        result <- "+"
      }else{
        result <- "-"
      }
    }else{
      result <- "0"
    }
    
    # print(reg)
    # plot(dataset)
    abline(reg, col="yellow", lwd=2)
  }
  
  
  # 	abline(reg2,col="red")
  return(result)
}

#Function for get the prefix from the pattern
KMP_Prefix <- function(pattern){
  
  #declare variable
  n_pattern <- length(pattern)
  prefix <- c(0)
  a <- 0
  
  #pattern making
  for(b in 2:n_pattern){
    while(a > 0 && pattern[a+1] != pattern[b]){
      a <- prefix[a]
    }
    if(pattern[a+1] == pattern[b]){
      a <- a+1
    }
    prefix[b] <- a
  }
  
  #return the result
  return(prefix)
  
}

#-------------------------------------------------------------------

#Function to seacrh the pattern in the string
KMP <- function(string, pattern){
  
  #inisiasi variabel
  prefix <- KMP_Prefix(pattern)
  n_string <- length(string)
  n_pattern <- length(pattern)
  index <- c()
  total <- 0
  i <- 0
  
  #Perulangan sesuai dengan jumlah string
  for(j in 1:n_string){
    while(i > 0 && pattern[i+1] != string[j]){
      i <- prefix[i]
    }
    if(pattern[i+1] == string[j]){
      i <- i+1
    }
    if(i == n_pattern){
      index <- c(index, j-n_pattern+1)
      total <- total+1
      i <- prefix[i]
    }
  }
  
  return(index)
  
}

MotifDiscoveryAnalysis <- function(colName, dataset, datasetIntervalValue){
  #list Parameter Categorical
  listParam <- mainConfig[which(mainConfig$Type == ("character") | mainConfig$Type == ("categorical")| mainConfig$Type == ("factor")),]$ColName
  result <- list()
  
  if(colName %in% listParam){
    n <- DataInterpreterInterval(datasetIntervalValue, type = "limit")
    index <- length(dataset)+1 - n
    
    #pattern
    pattern <- dataset[index:length(dataset)]
    
    #dataest
    dataset <- dataset[1:index]
    
    
    
    if(!is.null(KMP(dataset, pattern))){
      result$total <- length(KMP(dataset,pattern))
      result$pattern <- KMP(dataset,pattern)
    }else{
      result$total <- 0
      result$pattern <- NA
    }
  }else{
    result$total <- 0
    result$pattern <- NA
  }
  
  return(result)
}

ResumeEventExtreme <- function(datasetWithoutCatDate, statisticalResume, type=NULL){
  i <- 1
  vectorGrowth <- c()
  vectorStartIndex <- c()
  vectorEndIndex <- c()
  vectorInterpreter <- c()
  for(i in i:length(datasetWithoutCatDate)){
    listColumn <- datasetWithoutCatDate[[i]]
    if(type == "Growth"){
      listExtremeAnalysisResult <- ResumeHighestGrowthAnalysis(diff(listColumn),"Growth")
    }else if (type == "Decay"){
      listExtremeAnalysisResult <- ResumeHighestGrowthAnalysis(diff(listColumn),"Decay")
    }
    
    vectorGrowth[i] <-listExtremeAnalysisResult$valueResult
    vectorStartIndex[i] <-listExtremeAnalysisResult$startIndexResult
    vectorEndIndex[i] <-listExtremeAnalysisResult$endIndexResult
    
    #checking if range > 75% data range
    vectorInterpreter[i] <- InterpreterExtremeEvent(vectorGrowth[i], statisticalResume[i,])
  }
  #exception
  vectorEndIndex <- vectorEndIndex + 1
  
  #Combine all process into df
  dfExtremeEvent <- data.frame(vectorGrowth, vectorStartIndex, vectorEndIndex, vectorInterpreter)
  if(type == "Growth"){
    colnames(dfExtremeEvent) <- c("IncValue", "IncStartIndex", "IncEndIndex", "IncInterpreter")
  }else if (type == "Decay"){
    colnames(dfExtremeEvent) <- c("DecValue", "DecStartIndex", "DecEndIndex", "DecInterpreter")
  }
  
  return(dfExtremeEvent)
}

CorrelationAnalysis <- function(data){
  #using pearson correlation coefficient
  return(cor(data))
}

CurrentHighest <- function(data, statSummary, interval){
  analysisResult <- c()
  analysisIndex <- c()
  
  i <- 1
  result <- ""
  for(i in i:length(data)){
    #get highgest/lowest value from statsum
    maxVal <- as.numeric(as.character(statSummary[statSummary$ColName == names(data)[i],]$MaxValue))
    minVal <- as.numeric(as.character(statSummary[statSummary$ColName == names(data)[i],]$MinValue))
    
    sentence <- ""
    analysisResult[i] <- NA
    analysisIndex[i] <- NA
    if(data[i] >= maxVal){
      analysisResult[i] <- "+"
      analysisIndex[i] <- i
    }else if(data[i] <= minVal){
      analysisResult[i] <- "-"
      analysisIndex[i] <- i
    }
    result <- paste0(result, sentence)
  }
  
  result <- CurrentHighestAggregation(data, analysisResult, analysisIndex, interval)
  return(result)
}

ComparsionAnalysis <- function (data, dataset, interval){
  limit <- DataInterpreterInterval(interval, type = "lastlimit")
  datasetLimit<- dataset[nrow(dataset) - limit, !colnames(dataset) == "DateTime"]
  
  i<-1
  vectorResult <- c()
  for(i in i:length(data)){
    if(data[i] > datasetLimit[i]){
      vectorResult[i] <- "higher than"
    }else if(data[i] < datasetLimit[i]){
      vectorResult[i] <- "lower than"
    }else if(data[i] == datasetLimit[i]){
      vectorResult[i] <- "equal with"
    }
  }
  
  return(vectorResult)
}