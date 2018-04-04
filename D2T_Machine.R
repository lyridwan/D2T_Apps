 #Data-to-text System - Ahmad - Ridwan

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
library(plotrix)

#-----------------------------------------------------------------#

#----------------------- Load and transform -----------------------#
#Load dataset CLIMATE : Rainfall, Cloud Coverage, Temprature, Wind Speed, Wind Direction

dataset <- read.table(file="Datasets/dummy.csv", sep=",", header=TRUE)

#---------------------- Prediction with ETS and Gradient Descent ----------------------#
#Climate data prediction : Cloud Coverage,Average Temperature,Wind Speed,Wind Direction

#drops <- c("Date")
#datasetWithoutDate <- dataset[ , !(names(dataset) %in% drops)]

#-------------------------------------------------------------------------------#
#------------------------------- SIGNAL ANALYSIS -------------------------------
# Input : Numerical Sensor Data ( monitoring station of mabegondo, spain (MeteoGalicia))
# Output : Data Abstraction ( Weather Summary & Prediction)

#---------------------- Prediction with ETS and Gradient Descent
#Climate data prediction : Cloud Coverage,Average Temperature,Wind Speed,Wind Direction,Rainfall
#drop date

PredictDataset<-function(dataset){
drops <- c("Date")
datasetWithoutDate <- dataset[ , !(names(dataset) %in% drops)]
	i=1; n=length(datasetWithoutDate)
	x<-list()
	y<-matrix()
	for(i in i:n){
		#change dataset into time-series dataset (XTS)
		x[[i]] <- xts(datasetWithoutDate[[i]],dataset$Date)
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

predictionResult <- PredictDataset(dataset)

#Interval time on dataset
timeInterval <- difftime(dataset[nrow(dataset),"Date.Time"], dataset[1,"Date.Time"], units = "hours")

datasetColName <- c("")
datasetSumValue <- c("")

datasetMaxValue <- c("")
datasetMaxIndex <- c("")
datasetMaxDate <- c("")

datasetMinValue <- c("")
datasetMinIndex <- c("")
datasetMinDate <- c("")


i=2
n=length(dataset)
for(i in i:n){
	datasetColName[i] <- colnames(dataset[i])

	#MAX
	datasetMaxValue[i] <- max(dataset[i])
	max_index2 <- as.integer(which(dataset[i]==max(dataset[i])))
	datasetMaxIndex[i] <- max_index2[1]
	max_index0 <- max_index2[1]
	datasetMaxDate[i] <- as.character(dataset$Date[max_index0])

	#MIN
	datasetMinValue[i] <- min(dataset[i])
	min_index2 <- as.integer(which(dataset[i]==min(dataset[i])))
	datasetMinIndex[i] <- min_index2[1]
	min_index0 <- min_index2[1]
	datasetMinDate[i] <- as.character(dataset$Date[min_index0])

	#SUM
	datasetSumValue[i] <- sum(dataset[,i])
}

datasetMax <- data.frame(datasetColName, datasetMaxDate, datasetMaxValue);
datasetMin <- data.frame(datasetColName, datasetMinDate, datasetMinValue);
datasetSum <- data.frame(datasetColName, datasetSumValue);
datasetAverage <- data.frame(colMeans(dataset[2:6]));

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

AQValue <- AirQualityCalculation(airQualityDataset[1,]);


MembershipClassifier <- function(value, corpus){
  sapply(value, function(v) corpus[v >= corpus["Lower"] & v < corpus["Upper"],"Category"])
}

#TO BE UPDATED!!!!
MembershipFuzzy <- function(value, corpus){
  i <- 1;
  n <- nrow(corpus["Interval"]);
  print(n);
  interval <- vector("list", n);
  for (i in i:n) {
    interval[[i]] <- strsplit(as.character(corpus[i,"Interval"]), " ")[[1]];
    names(interval)[i] <- as.character(corpus[i, "Category"])
  }
  print(interval)
  return(interval)
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
DataInterpreterAdjective <- function(value, type="General"){
  if(type == "AirQuality" || type == "WindSpeed" || type == "WindDirection" || type == "CloudCoverage" || type == "Temperature")
    corpus <- read.table(file=paste0("Corpus/",type,"Adjective.csv"), sep=",", header=TRUE)
  if(type == "Temperature" || type == "Rainfall")
    result <- MembershipFuzzy(value, corpus);
  else
    result <- MembershipClassifier(value, corpus);
  else{
    corpus <- read.table(file="Corpus/GeneralAdjective.csv", sep=",", header=TRUE)
    result <- MembershipFuzzy(value, corpus);
  }
  
  print(result)
  return(result)
}

#dummyCorpus <- read.table(file=paste0("Corpus/","Temperature","Adjective.csv"), sep=",", header=TRUE)
#result <- MembershipFuzzy(10, dummyCorpus);

DataInterpreterAdjective(25, type = "AirQuality")
