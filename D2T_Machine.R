
#Data-to-text System - Ahmad - Ridwan
# setwd("~/GitHub/D2T_Apps")
#----------------------- Packages Requirement -----------------------#
#1. Shiny R
# install.packages(shiny) #-- server & GUI
library(shiny)

#2. Smooth 
# install.packages(smooth) -> #predicting  with ex.smoothing
library(smooth)

#3. Gradient Descent Packages 
# install.packages(gradDescent)
library(gradDescent)

#4. Time-Series data object
# install.packages(xts)
library(xts)

#4. Time-Series data object
#install.packages(sets)
library(sets)

#5. plotrix
#install.packages(plotrix)
library(plotrix)

#5. ade4
#install.packages(ade4)
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

PredictDataset<-function(dataset){
drops <- c("DateTime")
# datasetWithoutDate <- dataset[ , !(names(dataset) %in% drops)]
	datasetWithoutDate <- dataset[, names(dataset) != "DateTime"]
  i=1; n=length(datasetWithoutDate)
	x<-list()
	y<-matrix()
	for(i in i:n){
		#change dataset into time-series dataset (XTS)
    x[[i]] <- xts(datasetWithoutDate[[i]],order.by=as.POSIXct(dataset$DateTime))
		# x[[i]] <- xts(datasetWithoutDate[[i]],as.POSIXct(strptime(dataset$DateTime, "%Y-%m-%d %H:%M:%S")))
		#forecast with (ETS)
		y[i] <- forecast(x[[i]],h=1)$mean[1]
	}
	i=1
	for(i in i:n){
		names(y)[i]<-paste(colnames(datasetWithoutDate[i]))
	}
	y<-t(y)
	y<-data.frame(y)
	return(y)
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

# cat("------------        N-1 Dataset       -----------\n\n")
# datasetYesterday <- dataset[nrow(dataset)-1, !colnames(dataset)== "DateTime"]
# cat("------------        Last Dataset       -----------\n\n")
# datasetToday <- dataset[nrow(dataset), !colnames(dataset)== "DateTime"]
# cat("------------        LAST 2 MONTH AVERAGE       -----------\n\n")
# averageLast2Month <- as.data.frame.list(colMeans(dataset[(NROW(dataset)-60):(NROW(dataset)-30) , colnames(dataset) != "DateTime"]))
# print(averageLast2Month);
# cat("------------        LAST MONTH AVERAGE       -----------\n\n")
# averageLastMonth <- as.data.frame.list(colMeans(dataset[(NROW(dataset)-30):NROW(dataset) , colnames(dataset) != "DateTime"]))
# print(averageLastMonth);
# cat("------------        YEAR AVERAGE       -----------\n\n")
# averageYear <- as.data.frame.list(colMeans(dataset[, colnames(dataset) != "DateTime"]))
# print(averageYear);
# cat("------------        LAST 2 MONTH SUMMARY       -----------\n\n")
# statisticalLast2Month <- StatisticalAnalysis(dataset[(NROW(dataset)-60):(NROW(dataset)-30) , ])
# print(statisticalLast2Month);
# cat("------------        LAST MONTH SUMMARY       -----------\n\n")
# statisticalLastMonth <- StatisticalAnalysis(dataset[(NROW(dataset)-30):NROW(dataset) , ])
# print(statisticalLastMonth);
# cat("------------        YEAR SUMMARY       -----------\n\n")
# statisticalYear <- StatisticalAnalysis(dataset)
# print(statisticalYear);

#rm(datasetWithoutDate)
# datasetMin <- data.frame(datasetColName, datasetMinDate, datasetMinValue);
# datasetSum <- data.frame(datasetColName, datasetSumValue);
# datasetAverage <- data.frame(colMeans(dataset[2:6]));

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
    print("xx11")
  }
  else if((CO>5)&&(CO<=10)){
    b2<-10; b1<-5.1; a2<-100; a1<-51
    print("xx12")
  }
  else if((CO>10)&&(CO<=17)){
    b2<-17; b1<-10; a2<-200; a1<-101
    print("xx13")
  }
  else if((CO>17)&&(CO<=34)){
    b2<-34; b1<-17.1; a2<-300; a1<-201
    print("xx14")
  }
  else if((CO>34)&&(CO<=46)){
    b2<-46; b1<-34.1; a2<-400; a1<-301
    print("xx15")
  }
  else if((CO>46)&&(CO<=57.5)){
    b2<-57.5; b1<-46.1; a2<-500; a1<-401
    print("xx16")
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
   return (sapply(value, function(v) corpus[v >= corpus["Lower"] & v < corpus["Upper"],"Category"]))
}

#TO BE UPDATED!!!!
MembershipFuzzy <- function(value, corpus){
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
  membershipResult <- corpus[which.max(membershipValue), "Category"]
  return (membershipResult)
  # interval <- vector("list", n);
  # for (i in i:n) {
  #   interval[[i]] <- strsplit(as.character(corpus[i,"Interval"]), " ")[[1]];
  #   names(interval)[i] <- as.character(corpus[i, "Category"])
  # }
  # print(interval)
  # return(interval)
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
DataInterpreterAdjective <- function(value, type="General",statisticalResume){
  if(type == "AirQuality" || 
     type == "WindSpeed" || 
     type == "WindDirection" || 
     type == "CloudCoverage" ||
     type == "Temperature" || 
     type == "Rainfall"){
    corpus <- read.table(file=paste0("Corpus/",type,"Adjective.csv"), sep=",", header=TRUE)
    if(type == "Temperature" || type == "Rainfall"){
      result <- MembershipFuzzy(value, corpus);
    }else{
      result <- MembershipClassifier(value, corpus);
    }
  }else{
    corpus <- read.table(file=paste0("Corpus/GeneralAdjective.csv"), sep=",", header=TRUE)
    maxRange <- as.character(statisticalResume[statisticalResume$ColName == type,"MaxValue"])
    minRange <- as.character(statisticalResume[statisticalResume$ColName == type,"MinValue"])

    corpus <- read.table(file=paste0("Corpus/GeneralAdjective.csv"), sep=",", header=TRUE)
    maxRange <- as.character(statisticalResume[statisticalResume$ColName == "Tahu","MaxValue"])
    minRange <- as.character(statisticalResume[statisticalResume$ColName == "Tahu","MinValue"])

    maxRange <- as.double(maxRange)
    minRange <- as.double(minRange)

    if(minRange == maxRange){
      result <- "Constant"
    }else{
      n = nrow(corpus)
      node = (2*n)+n-1

      rangenode = (maxRange-minRange)/node

      value <- 7
      i=1
      j=0
      membershipValue <- c()
      for (i in i:n) {
        if(i == 1){
          v1<-minRange;
          v2<-minRange;
          v3<-minRange+(2*rangenode);
          v4<-minRange+(3*rangenode);
          
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
          j <- i+1
        }else{
          v1<-minRange+(j)*rangenode;
          v2<-minRange+(j+1)*rangenode;
          v3<-minRange+(j+3)*rangenode;
          v4<-minRange+(j+4)*rangenode;
          
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
          j <- j+3
        }
      }

      # result <- corpus[2, "Category"]
      result <- corpus[which.max(membershipValue), "Category"]
      # result<- paste("Not available for general -aa",which.max(membershipValue))
      ##corpus <- read.table(file="Corpus/GeneralAdjective.csv", sep=",", header=TRUE)
      ##result <- MembershipFuzzy(value, corpus);
    }

    
  }
  
  # print(result)
  return(result)
}

DataInterpreter <- function(dataset,statisticalResume){
  i <- 1;
  n <- length(dataset);
  interpreterResult <- dataset;
  for(i in i:n){
    interpreterResult[i] <- DataInterpreterAdjective(dataset[i], type = names(dataset[i]), statisticalResume)
    # print(DataInterpreterAdjective(dataset[i], type = names(dataset[i])))
  }
  
  # print(interpreterResult)
  return(interpreterResult)
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

Ordinal_indicator <- function(num){
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
  return(oi)
}

ReadIntro <- function(source="Data", type="General"){
  type
  if(type == "Current" || 
     type == "Trend" || type == "Event" ||
     type == "Predict"){
    corpus <- as.matrix(read.table(file=paste0("Corpus/",type,"Intro.csv"), header=FALSE, sep=';'))
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

ReadResumeIntro <- function(dataset, ColName, source="dataset"){
  
  corpus <- as.matrix(read.table(file=paste0("Corpus/","ResumeIntro.csv"), header=FALSE, sep=';'))

  
   
  #Randoming corpus
  n <- length(corpus)
  random_value <- as.integer(runif(1,1,n))
  result <- corpus[random_value]

  #Replaceing Data Source
  result <- gsub("@source", "dataset", result)

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
  i <- 2
  for (i in i:length(ColName)-1) {
    if(i == 2){
      param <- paste0(param,ColName[i])
    }
    else{
      param <- paste0(param,",",ColName[i])
    }
  # print(param)
  }
  param <- paste(param,"and",ColName[i+1])
  result <- gsub("@param", param, result)

  return (result)
}

ChangeTimeDesc <- function(source, dataset, type = "0"){
  n <- nrow(dataset)
  timeFirst <- as.character(dataset[n-1,'DateTime'])
  timeLast <- as.character(dataset[n,'DateTime'])

  timeFirst <- strptime(timeFirst, "%Y/%m/%d %H:%M:%OS")
  timeLast <- strptime(timeLast, "%Y/%m/%d %H:%M:%OS")

  now <- as.character(Sys.time())
  now <- strptime(now, "%Y-%m-%d")
  dayData <- as.character(dataset[length(dataset),'DateTime'])
  dayData <- strptime(dayData, "%Y/%m/%d")
  same <- 0
  
  if(type == "0"){
	  if(now == dayData){
	    same <- 1
	  }else{
	    same <- 0
	  }
  }else{
  	if(now == dayData){
	    same <- 1
 	}else{
		same <- 2
	}
  }

  difTime <- as.numeric(timeLast-timeFirst,units="secs")


  corpus <- read.table(file=paste0("Corpus/TimeDesc.csv"), sep=",", header=TRUE)
  timeDesc <- as.character(corpus[corpus$SecMin < difTime & corpus$SecMax >= difTime & corpus$Same == same,"Desc"])

  result <- gsub("@TimeDesc", timeDesc, source)
  return (result)
}

ResumeTrend <- function(){

}
ResumeIntroSentence <- function(){

}
ResumeEvent <- function(){

}

CurrentDesc <- function(interpreterNow,statisticalResume, dataset){
  result <- ''
  i <- 1
  for (i in i:length(interpreterNow)) {
    if(i == 1 || i == length(interpreterNow)){  
      result <- paste0(result, colnames(interpreterNow[i])," is ",interpreterNow[1,i])
    }else{
      result <- paste0(result,", ", colnames(interpreterNow[i])," is ",interpreterNow[1,i])
    }

    if(as.character(statisticalResume[statisticalResume$ColName == colnames(interpreterNow[i]),"MaxIndex"]) == as.character(nrow(dataset))){
      result <- paste0(result," this is the highest value of @TimeDesc ")
	}else if(as.character(statisticalResume[statisticalResume$ColName == colnames(interpreterNow[i]),"MinIndex"]) == as.character(nrow(dataset))){
      result <- paste0(result," this is the lowest value of @TimeDesc ")
	}

	if(i == length(interpreterNow)-1){
      result <- paste0(result," and ")
    }
  }

  if(grepl("@TimeDesc",result)){
  	result <- ChangeTimeDesc(result, dataset["DateTime"], type="other")
  }

  return(result)
}
CurrentAglast <- function(){

}
CurrentAgresume <- function(){

}

PredictIntro <- function(){

}
PredictContent <- function(){

}
PredictConc <- function(){

}

TrendAnalysis <- function(start,dataset){
  x = c(start:length(dataset))  
  #plot(sequence, dataset)
  
  reg = lm(dataset~x)
  if(reg$coefficients["x"] > 0 ){
    result <- "Increase"
  }else if(reg$coefficients["x"] < 0){
    result <- "Decrease"
  }else{
    result <- "Stable"
  }
  
  # abline(reg,col="red")
  
  return(result)
}

