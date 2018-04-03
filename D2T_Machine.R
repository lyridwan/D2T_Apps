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
