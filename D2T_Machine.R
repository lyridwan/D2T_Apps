
#Data-to-text System - Ahmad - Ridwan
#setwd("~/Programming/GitHub/D2T_Apps")
#----------------------- Packages Requirement -----------------------#
#1. Shiny R
# install.packages("shiny") #-- server & GUI
#library(shiny)

#2. Smooth 
# install.packages("smooth") -> #predicting  with ex.smoothing
library(smooth)

#3. Gradient Descent Packages 
# install.packages("gradDescent")
#library(gradDescent)

#4. Time-Series data object
# install.packages("xts")
library(xts)

#4. Time-Series data object
#install.packages("sets")
#library(sets)

#5. plotrix
#install.packages("plotrix")
library(greybox)
library(data.table)
#5. ade4
#install.packages("ade4")
# library(ade4)

#-----------------------------------------------------------------#

#----------------------- Load and transform -----------------------#
#Load dataset CLIMATE : Rainfall, Cloud Coverage, Temprature, Wind Speed, Wind Direction

# dataset <- read.table(file="Datasets/2016-2017.csv", sep=",", header=TRUE)
# airQualityDataset <- read.table(file="Datasets/AQ_2016_2017.csv", sep=",", header=TRUE)
# columnName <- colnames(dataset[ , colnames(dataset) != "DateTime"])
# dataset <- read.table(file="Datasets/dummy.csv", sep=",", header=TRUE)

#---------------------- Prediction with ETS and Gradient Descent ----------------------#
#Climate data prediction : Cloud Coverage,Average Temperature,Wind Speed,Wind Direction

# drops <- c("Date")
# datasetWithoutDate <- dataset[ , !(names(dataset) %in% drops)]

#-------------------------------------------------------------------------------#
#------------------------------- SIGNAL ANALYSIS -------------------------------
# Input : Numerical Sensor Data ( monitoring station of mabegondo, spain (MeteoGalicia))
# Output : Data Abstraction ( Weather Summary & Prediction)

#---------------------- Prediction with ETS and Gradient Descent
#Climate data prediction : Cloud Coverage,Average Temperature,Wind Speed,Wind Direction,Rainfall
#drop date
ClassHeaderChecker <- function(dataset){
  #dataset: vector
  
  # result: list of vector
  # example:
  #
  # $factor
  # [1] "DateTime"
  # 
  # $integer
  # [1] "CloudCoverage" "Temperature"   "WindDirection" "Rainfall"      "Price"         "Stock"         "Location"     
  # [8] "Sold"          "Sale"         
  # 
  # $numeric
  # [1] "WindSpeed"   "Tax"         "Probability"
  #
  result <- sapply(dataset, function(x) paste(class(x), collapse=" "))
  
  return(result)
}


ReadConfig <- function (){
  nullSequence <- rep(NA, length(columnName))
  dfResult <- data.frame(ColName = columnName, Type = nullSequence, Rule = nullSequence, Alternate=nullSequence, stringsAsFactors=FALSE)
  
  mainConfig <- read.table("Config/mainconfig.csv", header=TRUE, sep=",")
  
  #Merging Process with setting from file
  i<-1
  for(i in i:nrow(mainConfig)){
    tempColumn <- as.character(unlist(mainConfig$ColName[i]))
    dfResult[dfResult$ColName == tempColumn,] <- as.vector(unlist(mainConfig[mainConfig$ColName == tempColumn,]))
  }
  
  headerClass <- ClassHeaderChecker(dataset)
  headerClass <- headerClass[names(headerClass)!="DateTime"]
  i<-1
  for(i in i:length(headerClass)){
    tempColumn <- names(headerClass)[i]
    
    if(is.na(dfResult[dfResult$ColName == tempColumn,"Type"])){
      dfResult[dfResult$ColName == tempColumn,"Type"] <- headerClass[names(headerClass) == tempColumn]
    }
  }
  
  return(dfResult)
}

PredictDataset<-function(dataset, format="%m/%d/%Y %H:%M"){
  result <- c()
  lengthWithoutDate <-  length(dataset[,-which(colnames(dataset) == "DateTime")])
  
  dataSeries <- xts(dataset[,-which(colnames(dataset) == "DateTime")], order.by=as.Date(dataset[,"DateTime"], format))
  
  i<-1
  for(i in i:lengthWithoutDate){
    result[i] <- forecast(dataSeries[,i], h=1)$mean
  }
  
  
  names(result) <- colnames(dataset[ , colnames(dataset) != "DateTime"])
  
  # print(result)
  return(result)
}

# predictionResult <- PredictDataset(dataset)

# #Interval time on dataset
# timeInterval <- difftime(dataset[nrow(dataset),"Date.Time"], dataset[1,"Date.Time"], units = "hours")

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
    
    minRange <- as.double(maxRange)/2*-1
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
  #listGeneralPartition[[1]][1] <- maxRange*-1
  #listGeneralPartition[[length(listGeneralPartition)]][length(listGeneralPartition[[1]])] <- maxRange
  
  
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
    title <- paste(name," Membership Function")
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
      corpus <- read.table(file=paste0("Corpus/Range/",type,"Adjective.csv"), sep=",", header=TRUE)
      result <- MembershipClassifier(value, corpus);
    }
  }else{
    corpus <- GeneralFuzzyGenerator(type, statisticalResume)
    result <- MembershipFuzzy(value, corpus);
  }
  
  # print(result)
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

SubstrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
  }

OrdinalIndicator <- function(num){
  if(num==11){
    oi<-"th"
    return(oi)
  }
  x<-nchar(num)
  y<-substrRight(num,1)
  oi<-""
  if(y==1){
    oi<-"st"
  }
  else if(y==2){
    oi<-"nd"
  }
  else if(y==3){
    oi<-"rd"
  }
  else{
    oi<-"th"
  }
  return(paste0(num,oi))
}

DateInterval <- function(time1, time2){
  time1 <- strptime(time1, "%m/%d/%Y %H:%M")
  time2 <- strptime(time2, "%m/%d/%Y %H:%M")
  
  interval <- difftime(time1, time2)
  interval <- as.numeric(interval, units = "hours")
  
  return(interval)
}

DataInterpreterInterval <- function (interval, type = "default"){
  result<-""
  
  if(type == "interval"){
    if(interval == 1){
      result <- "hourly"
    }else if(interval == 24){
      result <- "daily"
    }else if(interval == 168){
      result <- "weekly"
    }else if(interval == 672 || interval == 696 || interval == 720 || interval == 744){
      result <- "monthly"
    }else if(interval == 8760 || interval == 8736){
      result <- "yearly"
    }else{
      result <- ""
    }
  }else if(type == "intro"){
    if(interval == 1){
      result <- "This hour"
    }else if(interval == 24){
      result <- "Today"
    }else if(interval == 168){
      result <- "This week"
    }else if(interval == 720 || interval == 744){
      result <- "This month"
    }else if(interval == 8760 || interval == 8736){
      result <- "This year"
    }else{
      result <- ""
    }
    if(interval == "hourly"){
      n <- 6
    }else if(interval == "daily"){
      n <- 7
    }else if(interval == "weekly" || interval == "monthly" ||interval == "yearly"){
      n <- 4
    }
  }else if(type == "limit"){
    #hourly data
    if(interval == 1){
      result <- 6
    #daily data
    }else if(interval == 24){
      result <- 7
    #monthly&yearly data
    }else if(interval == 168 || 
             interval == 720 || 
             interval == 744 || 
             interval == 8760 || 
             interval == 8736){
      result <- 4
    }else{
      result <- ""
    }
  }else{
    if(interval == 1){
      result <- "hour"
    }else if(interval == 24){
      result <- "day"
    }else if(interval == 168){
      result <- "week"
    }else if(interval == 720 || interval == 744){
      result <- "month"
    }else if(interval == 8760 || interval == 8736){
      result <- "year"
    }else{
      result <- ""
    }
  }
  
  return(result)
}

#------------------------------------------------------------------------------------------------------------------
# INTRO
#------------------------------------------------------------------------------------------------------------------


ReadIntro <- function(type="General"){
  type
  if(type == "Current" || 
     type == "Trend" || type == "Event" ||
     type == "Predict" || type == "PredictConj"  ||
     type == "Temperature" || type == "AirQuality"  ){
    corpus <- as.matrix(read.table(file=paste0("Corpus/",type,"Intro.csv"), header=FALSE, sep=';', quote=""))
    # print(corpus)
    n <- length(corpus)
    random_value <- as.integer(runif(1,1,n+0.5))

    result <- corpus[random_value]
    return (result)
  }else{
    return("Woops no data intro!");
  }
    # return("Woops no data intro!");
}

ReadResumeIntro <- function(dataset, ColName, source="data"){
  
  corpus <- as.matrix(read.table(file=paste0("Corpus/","ResumeIntro.csv"), header=FALSE, sep=';'))

  #Randoming corpus
  n <- length(corpus)
  random_value <- as.integer(runif(1,1,n))
  result <- corpus[random_value]

  #Replaceing Data Source
  result <- gsub("@source", source, result)
  
  #Replaceing Data Source
  interval <- DataInterpreterInterval(datasetIntervalValue, type = "interval")
  result <- gsub("@interval", interval, result)

  #Replacing Data Range
  date1 <- dataset[1, ]
  date2 <- dataset[nrow(dataset),]
  result <- gsub("@date1", date1, result)
  result <- gsub("@date2", date2, result)

  #Replacing prural identifier
  if(length(ColName) == 1){
    result <- gsub("parameter@s", "parameter:", result)
  }else{
    result <- gsub("parameter@s", "parameters:", result)
  }

  #Replacing Parameter with array
  param <- ""
  i <- 1
  for (i in i:length(ColName)-1) {
    if(i == 1){
      param <- paste0(ColName[i])
    }
    else{
      param <- paste0(param,", ",ColName[i])
    }
  # print(param)
  }
  param <- paste0(param,", and ",ColName[i+1])
  result <- gsub("@param", param, result)
  
  return (result)
}

ReadCurrentIntro <- function(dateTime){
  corpus <- as.matrix(read.table(file=paste0("Corpus/CurrentIntro.csv"), header=FALSE , sep = ';'))
  
  n <- length(corpus)
  random_value <- as.integer(runif(1,1,n+0.5))
  
  result <- corpus[random_value]
  
  interval <- DataInterpreterInterval(datasetIntervalValue, "intro")
  result <- gsub("@date", interval, result)
  
  result <- gsub("@time", dateTime, result)
  
  return(result)
}

#------------------------------------------------------------------------------------------------------------------
# RESUME ANALYSIS
#------------------------------------------------------------------------------------------------------------------


ResumeTrend <- function(statisticalResume){
  freq <- table(statisticalResume["Trend"])
  result <- ""
  
  #FIRST CONDITION
  if(!is.na(freq["0"]) && !is.na(freq["+"]) && !is.na(freq["-"])){
    if(freq["+"] <= freq["-"] && freq["+"] <= freq["0"]){
      if(freq["-"] < freq["0"]){
        #1ST GROUP
        listTrend <- statisticalResume[statisticalResume$Trend == "+", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is increased", "but ")
        
        #2ND GROUP
        listTrend <- statisticalResume[statisticalResume$Trend == "-", ]
        i<-1
        for(i in i:n){
          
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is decreased and the rest is constant.")
      }else{
        #1ST GROUP
        listTrend <- statisticalResume[statisticalResume$Trend == "+", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is increased", "and ")
        
        #2ND GROUP
        listTrend <- statisticalResume[statisticalResume$Trend == "0", ]
        i<-1
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is constant but the rest is decreased.")
      }
      #SECOND CONDITION
    }else if(freq["-"] <= freq["+"] && freq["-"] <= freq["0"]){
      if(freq["+"] < freq["0"]){
        listTrend <- statisticalResume[statisticalResume$Trend == "-", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is decreased", "but ")
        
        listTrend <- statisticalResume[statisticalResume$Trend == "+", ]
        i<-1
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is increased and the rest is constant.")
      }else{
        listTrend <- statisticalResume[statisticalResume$Trend == "-", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is decreased", "and ")
        
        listTrend <- statisticalResume[statisticalResume$Trend == "0", ]
        i<-1
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is constant but the rest is increased.")
      }
    }else if(freq["0"] <= freq["-"] && freq["0"] <= freq["+"]){
      if(freq["+"] < freq["-"]){
        listTrend <- statisticalResume[statisticalResume$Trend == "0", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is constant", "and ")
        
        listTrend <- statisticalResume[statisticalResume$Trend == "+", ]
        i<-1
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is increased and the rest is decreased.")
      }else{
        listTrend <- statisticalResume[statisticalResume$Trend == "0", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is constant", "and ")
        
        listTrend <- statisticalResume[statisticalResume$Trend == "-", ]
        i<-1
        for(i in i:n){
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is decreased but the rest is increased.")
      }
    }
  }else{
    if(is.na(freq["0"]) && is.na(freq["+"])){
      # ALL -
      result <- "Trend of all variable is decreased."
    }else if(is.na(freq["0"]) && is.na(freq["-"])){
      # ALL +
      result <- "Trend of all variable is increased."
      
    }else if(is.na(freq["+"]) && is.na(freq["-"])){
      # ALL 0
      result <- "Trend of all variable is constant."
      
    }else if(is.na(freq["0"])){
      # Only + -
      if(freq["-"] > freq["+"]){
        #more 0
        listTrend <- statisticalResume[statisticalResume$Trend == "+", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is increased", "but the rest is decreased.")
      }else{
        #more +
        listTrend <- statisticalResume[statisticalResume$Trend == "-", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is decreased", "but the rest is increased.")
        
      }
    }else if(is.na(freq["+"])){
      # Only 0 -
      if(freq["0"] > freq["-"]){
        #more 0
        listTrend <- statisticalResume[statisticalResume$Trend == "+", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is decreased", "and the rest is constant.")
      }else{
        #more +
        listTrend <- statisticalResume[statisticalResume$Trend == "0", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is constant", "and the rest is decreased.")
        
      }
    }else if(is.na(freq["-"])){
      # Only 0 +
      if(freq["0"] > freq["+"]){
        #more 0
        listTrend <- statisticalResume[statisticalResume$Trend == "+", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is increased", "and the rest is constant.")
      }else{
        #more +
        listTrend <- statisticalResume[statisticalResume$Trend == "0", ]
        i<-1
        n <- nrow(listTrend)
        for(i in i:n){
          
          if(i==1){
            result <- paste0(result,listTrend[i,"ColName"])
            
          }else{
            result <- paste0(result,", ",listTrend[i,"ColName"])
          }
        }
        
        result <- paste(result, "trend is constant", "and the rest is increased.")
        
      }
    }
  }
  
  return(result)   
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
    dateStart <- dataset[["DateTime"]][vectorStart[i]]
    dateEnd <- dataset[["DateTime"]][vectorEnd[i]]
    
    dateRange <- LexicalDateRange(dateStart,dateEnd)
    
    subSentence <- paste(dateRange, colName, "stayed constant at", dataset[[colName]][vectorStart[i]])
    subSentence <- paste0(subSentence, " (", interpreterResult[i],  ")")
    mainSentence <- paste0("During ", subSentence,".")
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
      
      subSentence <- paste(subSentence, colName, "stayed constant at", dataset[[colName]][vectorStart[i]])
      subSentence <- paste0(subSentence, " (", interpreterResult[i],  ")")
      mainSentence <- paste0(mainSentence, "During ", subSentence,".")
      i <- i + 1
    }
    
  }
  
  return(mainSentence)
}#end function


LexicalDateRange  <- function(dateStart, dateEnd){
  #FORMAT: mm/dd/yyyy -> "07/01/2018"
  
  # print(dateStart)
  # print(dateEnd)
  startMonth <- as.numeric(substr(dateStart,1,2))
  endMonth <- as.numeric(substr(dateEnd,1,2))
  
  startDate <- as.numeric(substr(dateStart,4,5))
  endDate <- as.numeric(substr(dateEnd,4,5))
  
  startYear <- as.numeric(substr(dateStart,7,10))
  endYear <- as.numeric(substr(dateEnd,7,10))
  
  timeRepeated <- ""
  if(startYear == endYear){
    if(startMonth == endMonth){
      #Example: 1 - 8 June 2018
      timeRepeated <-paste0(startDate, "-", endDate, " ", month.abb[startMonth], " ", startYear)
      
    #if month different
    }else{
      #Example: 1 Mar - 8 June 2018
      timeRepeated <-paste(startDate, month.abb[startMonth], "-",  endDate, month.abb[endMonth], startYear)
    }
  }else{
    #Example: 20 Jan 2017 - 08 Jan 2018
    timeRepeated <-paste(startDate, month.abb[startMonth], startYear, "-", endDate, month.abb[endMonth], endYear)
  }
  
  return(timeRepeated)
}

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
              if(interpreterResult$InterpreterResult[i] == interpreterResult$InterpreterResult[j] && vectorTrendDesc[i] == vectorTrendDesc[j]){
                #mark the index as checked
                reps[j] <- j
                
                colName <- interpreterResult$Colname[j]
                
                #isGroupingAvailable: Vector, TRUE OR FALSE
                #TRUE FALSE
                #0    0
                indicator <- c("TRUE"=0, "FALSE"=0)
                isGroupingAvailable <- (interpreterResult$InterpreterResult[j:length(interpreterResult$InterpreterResult)] == interpreterResult$InterpreterResult[i]) & vectorTrendDesc[j:length(vectorTrendDesc)] == vectorTrendDesc[i]
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
        
        #after groping the column
        interpreter <- interpreterResult$InterpreterResult[i]
        if(interpreter == "Constant"){
          phrase <- change_word_bank_AQ2()
          interpreter = "from the first time"
        }else{
          phrase <- change_word_bank_AQ(vectorTrendDescriptionAnalysis[i])
        }
        
        subSentence <- paste(subSentence, phrase, interpreter)
        mainSentence <- paste0(mainSentence, subSentence,". ")
      }
      # cat("reps:", i, " ",reps,"\n")
      
      i <- i + 1
    }
    
  }#end if repValue
  
  return(mainSentence)
}

ReadPredictIntro <-function (intro) {
  conj <- ReadIntro(type="PredictConj")
  
  intro <- gsub("@conj",conj, intro)
  
  return(intro)
}

PredictDesc <- function(interpreterResult, vectorTrendDesc, dataset){
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
              if(interpreterResult$InterpreterResult[i] == interpreterResult$InterpreterResult[j] && vectorTrendDesc[i] == vectorTrendDesc[j]){
                #mark the index as checked
                reps[j] <- j
                
                colName <- interpreterResult$Colname[j]
                
                #isGroupingAvailable: Vector, TRUE OR FALSE
                #TRUE FALSE
                #0    0
                indicator <- c("TRUE"=0, "FALSE"=0)
                isGroupingAvailable <- (interpreterResult$InterpreterResult[j:length(interpreterResult$InterpreterResult)] == interpreterResult$InterpreterResult[i]) & vectorTrendDesc[j:length(vectorTrendDesc)] == vectorTrendDesc[i]
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
        
        #after grouping the column
        interpreter <- interpreterResult$InterpreterResult[i]
        if(interpreter == "Constant"){
          phrase <- change_word_bank_AQ2()
          interpreter = "from the first time"
        }else{
          phrase <- change_word_bank_AQ(vectorTrendDescriptionAnalysis[i])
        }
        
        subSentence <- paste(subSentence, "will", phrase, interpreter)
        mainSentence <- paste0(mainSentence, subSentence,". ")
      }
      # cat("reps:", i, " ",reps,"\n")
      
      i <- i + 1
    }
    
  }#end if repValue
  
  return(mainSentence)
}

PredictContent <- function(interpreterResult, vectorTrendDesc, dataset){
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
              if(interpreterResult$InterpreterResult[i] == interpreterResult$InterpreterResult[j] && vectorTrendDesc[i] == vectorTrendDesc[j]){
                #mark the index as checked
                reps[j] <- j
                
                colName <- interpreterResult$Colname[j]
                
                #isGroupingAvailable: Vector, TRUE OR FALSE
                indicator <- c("TRUE"=0, "FALSE"=0)
                isGroupingAvailable <- (interpreterResult$InterpreterResult[j:length(interpreterResult$InterpreterResult)] == interpreterResult$InterpreterResult[i]) & vectorTrendDesc[j:length(vectorTrendDesc)] == vectorTrendDesc[i]
                isGroupingAvailable <- table(isGroupingAvailable)
                
                #handling if table dont have FALSE or TRUE value, then use default value (0)
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
        
        #after groping the column
        interpreter <- interpreterResult$InterpreterResult[i]
        phrase <- change_word_bank_AQ(vectorTrendDescriptionAnalysis[i])
        
        subSentence <- paste(subSentence, phrase, interpreter)
        mainSentence <- paste0(mainSentence, subSentence,". ")
      }
      # cat("reps:", i, " ",reps,"\n")
      
      i <- i + 1
    }
    
  }#end if repValue
  
  return(mainSentence)
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
	   abline(reg,col="red")
  }
  

# 	abline(reg2,col="red")
  return(result)
}

#----------------------------- Microplanning for Prediction ------------------------------------------# 

# Lexicalisation proses

# Source = Ramos
# Function

LD_Compare <- function (index_data){
  #Compute Index Variation
  i=1; n=length(index_data); IV<-c(0,0);
  for(i in i:n){
    if(i<n){
      IV[i]<-((index_data[[i+1]])-(index_data[[i]]))	
    }else{
      IV[i]<-index_data[[i]]
      
    }
  }
  
  #Apply Rules
  i=1; IVL<-c(0,0)
  for(i in i:n){
    if(IV[i]>0){
      IVL[i]="+"
    }else if(IV[i]<0){
      IVL[i]="-"
    }else{
      IVL[i]="0"
    }
  }
  # print(IVL)
  x<-TrendDesc_template(IVL)
  return(x)
}

TrendDesc_template <- function (IVL,data){
  if((IVL[1]=="0")&&(IVL[2]=="0")){
    TrendDesc <- "stable"
  }
  if(((IVL[1]=="+")&&(IVL[2]=="-"))||((IVL[1]=="-")&&(IVL[2]=="+"))){
    TrendDesc <- "mediumChange"
  }
  if(((IVL[1]=="+")&&(IVL[2]=="0"))||((IVL[1]=="-")&&(IVL[2]=="0"))){
    TrendDesc <- "startChange"
  }
  if(((IVL[1]=="0")&&(IVL[2]=="+"))||((IVL[1]=="0")&&(IVL[2]=="-"))){
    TrendDesc <- "endChange"
  }
  if(((IVL[1]=="+")&&(IVL[2]=="+"))||((IVL[1]=="-")&&(IVL[2]=="-"))){
    TrendDesc <- "progressiveChange"
  }
  return(TrendDesc)
}

change_word_bank_AQ <- function (fragmentCode){
  phraseAQ <- read.table(file="wordbank/AQ_phrase_bank.csv", sep=",", header=TRUE)
  n=length(phraseAQ); i=1; 
  for(i in i:n){
    m=colnames(phraseAQ[i])
    if(fragmentCode==m){
      j=runif(1,1,n+1)
      return(phraseAQ[j,i])
    }
  }
}

change_word_bank_AQ2 <- function (){
  phrase <- as.matrix(read.table("Corpus/AQ_phrase_bank2.csv", header=FALSE, sep=';', quote=""))
  # print(corpus)
  n <- length(phrase)
  random_value <- as.integer(runif(1,1,n+0.5))
  
  result <- phrase[random_value]
  return (result)
}

change_word_bank_AQ3 <- function (fragmentCode){
  phraseAQ <- read.table(file="Corpus/AQ_phrase_bank3.csv", sep=",", header=TRUE)
  n=length(phraseAQ); i=1; 
  for(i in i:n){
    m=colnames(phraseAQ[i])
    if(fragmentCode==m){
      j=runif(1,1,n+1)
      return(phraseAQ[j,i])
    }
  }
}

DocPlanHighestGrowthDecay <- function (dateTime, dfGrowth, type){
  if(length(dfGrowth) != 0){
    result <- c()
    #JPY increased greatly (4.3530 point) from 1st Aug to 1st Oct 2008
    
    i<-1
    for(i in i:nrow(dfGrowth)){
      # index <- rownames(dfGrowth)[i]
      
      sentence <- ""
      if(type == "Growth"){
        phrase <- "increased greatly"
      }else if(type == "Decay"){
        phrase <- "decreased greatly"
      }
      # print(dfGrowth$vectorStartIndex[1])
      # print(dfGrowth$vectorEndIndex[1])
      # 
      # print(as.character(dateTime[dfGrowth$vectorStartIndex[i]]))
      # print(as.character(dateTime[dfGrowth$vectorEndIndex[i]]))
      
      dateRange <- LexicalDateRange(as.character(dateTime[dfGrowth$vectorStartIndex[i]]), as.character(dateTime[dfGrowth$vectorEndIndex[i]]))
      # print(dateRange)
      
      sentence <- paste0(dfGrowth$colName[i], " ", phrase, " (", dfGrowth$vectorGrowth[i]," points) from ", dateRange)
      
      result[i] <- as.character(sentence)
    }
  }else{
    result <- ""
  }
  
  return(result)
}

AggResumeGrowth <- function(vectorGrowth, vectorDecay){
  i<-1
  
  sentence1 <- ""
  if(length(vectorGrowth) != 0){
    for(i in i:length(vectorGrowth)){
      if(i == length(vectorGrowth) && i != 1){
        sentence1 <- paste0(sentence1, " and ", vectorGrowth[i], ".")
      }else{
        sentence1 <- paste0(sentence1, vectorGrowth[i], ", ")
      }
    }
  }
  
  sentence2 <- ""
  if(length(vectorDecay) != 0){
    sentence2 <- "While "
    i<-1
    for(i in i:length(vectorDecay)){
      if(i == length(vectorDecay) && i !=1){
        sentence2 <- paste0(sentence2, "and ", vectorDecay[i], ".")
      }else{
        sentence2 <- paste0(sentence2, vectorDecay[i], ", ")
      }
    }
  }
  
  result <- paste0(sentence1, sentence2)
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
  print(dataset)
  n <- DataInterpreterInterval(datasetIntervalValue, type = "limit")
  index <- length(dataset)+1 - n
  
  #pattern
  pattern <- dataset[index:length(dataset)]
  
  #dataest
  dataset <- dataset[1:index]
  print(dataset)
  print(pattern)
  # print(length(pattern))
  
  result <- list()
  if(!is.null(KMP(dataset, pattern))){
    result$total <- length(KMP(dataset,pattern))
    result$pattern <- KMP(dataset,pattern)
  }else{
    result$total <- 0
    result$pattern <- NA
  }
  
  return(result)
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

MotifDiscoveryDocPlan <- function(listMD){
  listCategorical <- mainConfig[which(mainConfig$Type == ("integer") | mainConfig$Type == ("categorical")),]
  
  
  if(nrow(listCategorical) != 0){
    
    listColumn <- rownames(listCategorical)
    
    i<-1
    for(i in i:length(listColumn)){
      if(is.na(listMD[[as.numeric(listColumn[i])]]$pattern[1])) {
        listColumn[i] <- NA
      }
    }
  }else{
    listColumn <- NULL
  }
  
  return(listColumn)
}

MotifDiscoveryMicroPlan <- function(listColumn, listMD){
  # > listColumn
  # [1] "6" NA  NA  NA  NA
  
  # > listMD
  # [[6]]
  # [[6]]$`total` 
  # [1] 9
  # 
  # [[6]]$pattern
  # [1] 10 11 12 13 14 15 16 17 18
  # 
  # 
  # [[7]]
  # [[7]]$`total`
  # [1] 0
  # 
  # [[7]]$pattern
  # [1] NA
  
  limit <- DataInterpreterInterval(datasetIntervalValue, type = "limit")
  interval <- paste0(DataInterpreterInterval(datasetIntervalValue, type = "default"), "s")
  
  
  #If there's no pattern match
  MDcontent <- ""
  if(sum(!is.na(listColumn)) == 0){
    verb <- MotifDiscoveryRE()
    MDintro <- paste0("For the past ", limit, " ", interval, " ,")
    MDcontent <- paste("no", verb, "patterns were found for each categorical/integer parameters.")
    
    MDsentence <- paste(MDintro, MDcontent)
    return(MDsentence)
  #Aggregation
  }else{
    #There are a matching USD data pattern in the last 7 days 
    #with data patterns from August 22-24 2018 and September 23-25 2018.
    
    #if pattern found only 1 parameter
    if(sum(!is.na(listColumn)) == 1){
      selectedColindex <- as.numeric(listColumn[which(!is.na(listColumn))])
      selectedColname <- columnName[selectedColindex]
      verb <- MotifDiscoveryRE()
      
      dateAggregation <- ""
      i <- 1
      #Iterasi sebanyak pattern yang ada dalam list
      for(i in i:length(listMD[[selectedColindex]]$pattern)){
        indexMD <- as.numeric(listMD[[selectedColindex]]$pattern[i])
        startDate <- dataset[indexMD,"DateTime"]
        endDate <- dataset[indexMD + limit,"DateTime"]
        
        dateRange <- LexicalDateRange(startDate,endDate)
        
        #first
        if(i == 1){
          dateAggregation <- paste0(dateAggregation, dateRange)
        #middle
        }else if(i == length(listMD[[selectedColindex]]$pattern)){
          dateAggregation <- paste0(dateAggregation, ", and ", dateRange, ".")
        #lastcondition
        }else{
          dateAggregation <- paste0(dateAggregation, ", ", dateRange)
        }
      }
      
      #single pattern found
      if(length(listMD[[selectedColindex]]$pattern) == 1){
        tobe <- "is"
        s <- ""
      }else{
        tobe <- "are"
        s <- "s"
      }
      
      #a or an replace
      if(verb == "identical"){
        a <- "an"
      }else{
        a <- "a"
      }
      
      dateRangedataset <- LexicalDateRange(dataset[nrow(dataset)-limit, "DateTime"], dataset[nrow(dataset), "DateTime"])
      
      MDcontent <- paste("There @tobe @a", verb, selectedColname, "data pattern", "in the last", limit, interval,
                         "(@dateNow)", "with data pattern@s from", dateAggregation)
      MDcontent <- gsub("@tobe", tobe, MDcontent)
      MDcontent <- gsub("@s", s, MDcontent)
      MDcontent <- gsub("@a", a, MDcontent)
      MDcontent <- gsub("@dateNow", dateRangedataset, MDcontent)
      return(MDcontent)
    }else{
      #removing NA value
      
      
      listColumn2 <- listColumn[which(!is.na(listColumn))]
      
      listColumnName <- columnName[as.numeric(listColumn2)]
      
      MDcontent <- ""
      groupColumn <- ""
      i<-1
      for(i in i:length(listColumnName)){
        #INTRO
        if(i != length(listColumnName)){
          groupColumn <- paste0(groupColumn, listColumnName[i], ", ")
        }else{
          groupColumn <- paste0(groupColumn, "and ", listColumnName[i])
        }
        
        #CONTENT
        selectedColindex <- as.numeric(listColumn2[i])
        dateAggregation <- ""
        j <- 1
        #Iterasi sebanyak pattern yang ada dalam list
        for(j in j:length(listMD[[selectedColindex]]$pattern)){
          indexMD <- as.numeric(listMD[[selectedColindex]]$pattern[j])
          startDate <- dataset[indexMD,"DateTime"]
          endDate <- dataset[indexMD + limit,"DateTime"]
          
          dateRange <- LexicalDateRange(startDate,endDate)
          
          #first
          if(j == 1){
            dateAggregation <- paste0(dateAggregation, dateRange)
            #middle
          }else if(j == length(listMD[[selectedColindex]]$pattern)){
            dateAggregation <- paste0(dateAggregation, ", and ", dateRange, ". ")
            #lastcondition
          }else{
            dateAggregation <- paste0(dateAggregation, ", ", dateRange)
          }
        }
        
        #USD data pattern matches with the data pattern from August 22
        MDcontent <- paste0(MDcontent, listColumnName[i], " data pattern matches with their data from ", dateAggregation)
      }
      #USD and JPY data patterns match with their last 7 days data pattern.
      MDintro <- paste(groupColumn, "data patterns match with their last", limit, interval, "data pattern.")
      MDsentence <- paste(MDintro, MDcontent)
      return(MDsentence)
    }
  }
}

#Motif Discovery Reffering Expressions
MotifDiscoveryRE <- function (){
  phrase <- as.matrix(read.table("Corpus/MotifDiscoveryRE.csv", header=FALSE, sep=';', quote=""))
  # print(corpus)
  n <- length(phrase)
  random_value <- as.integer(runif(1,1,n+0.5))
  
  result <- phrase[random_value]
  return (result)
}


MissingValueHandling <- function(dataset){
  md.pattern(dataset)
  
  #using linear regression
  model <- mice(dataset,maxit=50,seed=500, meth="norm")
  result <- complete(model)  # generate the completed data.
  
  return(result)
}

IsSpecialCorpusAvailable <- function(interpreterPredict, colName){
  case1 <- c("Rainfall","CloudCoverage")
  case2 <- c("Temperature")
  case3 <- c("CO","NO","NO2","NOX","O3","PM10","PM25","SO2")
  
  boolcase1 <- case1 %in% mainConfig[!is.na(mainConfig$Rule),]$ColName
  boolcase2 <- case2 %in% mainConfig[!is.na(mainConfig$Rule),]$ColName
  boolcase3 <- case3 %in% mainConfig[!is.na(mainConfig$Rule),]$ColName
  
  result <- NULL
  vectorResult <- NULL
  #CASE 1: (SKY STATE SENTENCE, RAINFALL & CLOUD COVERAGE)
  if(sum(boolcase1 == "FALSE") == 0){
    #get the rainstate from list
    rainState <- lapply(interpreterPredict,`[[`, which(interpreterPredict$Colname == "Rainfall"))$InterpreterResult
    cloudState <- lapply(interpreterPredict,`[[`, which(interpreterPredict$Colname == "CloudCoverage"))$InterpreterResult
    
    skyState <- skyStateAgg(rainState, cloudState)
    # skyIntro <- ReadIntro(type="Predict")
    # skyIntro <- gsub("@conj", "tomorrow sky will be")
    skyIntro <- "tomorrow sky will be"
    skySentence <- paste(skyIntro, skyState)
    
    result <- paste(skySentence)
    vectorResult <- case1
  }
  
  #CASE 2: (TEMPERATURE SENTENCE)
  if(sum(boolcase2 == "FALSE") == 0){
    #get all value when list$colname value == temperature
    temperatureState <- lapply(interpreterPredict,`[[`, which(interpreterPredict$Colname == "Temperature"))$InterpreterResult
    
    temperatureIntro <- ReadIntro(type = "Temperature")
    temperatureTrendDesc<- TrendDescTemperature()
    temperatureSentence <- paste(temperatureIntro, temperatureTrendDesc, temperatureState)
    temperatureSentence <- paste0(temperatureSentence,".")
    
    result <- paste(result, temperatureSentence)
    vectorResult <- c(vectorResult, case2)
  }
  
  #CASE3: (AIR QUALITY SENTENCE)  
  if(sum(boolcase3 == "FALSE") == 0){
    AQdataLast <- datas[-1,case3]
    AQdatanow <- datas[nrow(datas),case3]
    AQdataPredict <- PredictDataset(datas, "%d/%m/%Y")
    
    AQvalLast <- AirQualityCalculation(AQdataLast)
    AQvalnow <- AirQualityCalculation(AQdatanow)
    AQvalPredict <- AirQualityCalculation(AQdataPredict)
    
    AQInterpreterLast <- DataInterpreterAdjective(AQvalLast, type="AirQuality")$InterpreterIndex
    AQInterpreterNow <- DataInterpreterAdjective(AQvalnow, type="AirQuality")$InterpreterIndex
    AQInterpreterPredict <- DataInterpreterAdjective(AQvalPredict, type="AirQuality")$InterpreterIndex
    
    AQsequence <- LD_Compare(c(AQvalLast, AQvalnow, AQvalPredict))
  
    AQintro <- ReadIntro(type="AirQuality")
    AQtrendDesc <- change_word_bank_AQ(AQsequence)
    AQstate<- DataInterpreterAdjective(AQvalPredict, type="AirQuality")$InterpreterResult
    
    AQsentence <- paste(AQintro, AQtrendDesc, AQstate)
    
    result <- paste(result, AQsentence)
    vectorResult <- c(vectorResult, case3)
  }
  
  # print(result)
  # print(vectorResult)
  return(list(Sentence = result, VectorResult = vectorResult))
}

#Function Sky State Aggregation with Simple Conjunction
skyStateAgg <- function (rain,cloud){
  #Assign Rule for Contrast Value for each partition of rain state
  if(rain=="no rain"||rain=="light rain"){
    Contrast1=0
  }
  else if(rain=="moderate rain"||rain=="heavy rain" ||
          rain=="intense rain" || rain=="torential rain"){
    Contrast1=1
  }
  #Assign Rule for Contrast Value for each partition of cloud state
  if(cloud=="clear"||cloud=="foggy"||cloud=="mostly sunny"){
    Contrast2=0
  }
  else if(cloud=="partly cloudy"||cloud=="mostly cloudy"||cloud=="broken"
          || cloud=="overcast"){
    Contrast2=1
  }
  
  if(Contrast1==Contrast2){
    
    Conjunction<-"covered with"
  }else{
    
    Conjunction<- "although it's covered by"
    
  }
  
  result <- paste(rain,Conjunction,cloud,"sky.")
  return(result)
}

TrendDescTemperature <- function(){
  var2 <- datasetNow[,"Temperature"]
  var1 <- datasetPredicted["Temperature"]
  
  if(var2>var1){
    return("increased to")
  }
  else if(var2<var1){
    return("decreased to")
  }
  else{
    x<-as.integer(runif(1,1,4))
    if(x==1){
      return("keep stable at")
    }
    if(x==2){
      return("stay stable at")
    }
    if(x==3){
      return("constant at")
    }
  }
}


PostProcessing <- function(corpus){
  result <- mainConfig[!is.na(mainConfig$Alternate),]
  
  i<-1
  for(i in i:nrow(result)){
    corpus <- gsub(as.character(result$ColName[i]), as.character(result$Alternate[i]), corpus)
  }
  
  return(corpus)
}
