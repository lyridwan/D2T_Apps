 #Data-to-text System - Brahma Putra

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
dataClimates <- read.table(file="Datasets/2016-2017.csv", sep=",", header=TRUE)

#transform "Date" Variable from integer to date
dataClimates <- transform(dataClimates, Date = as.Date(as.character(Date), "%d/%m/%Y"))

#Load dataset AirQuality
dataAQ <- read.table(file="Datasets/AQ_2016_2017.csv", sep=",", header=TRUE)

#transform "Date" Variable from integer to date
dataAQ <- transform(dataAQ, Date = as.Date(as.character(Date), "%d/%m/%Y"))
#-----------------------------------------------------------------#

#---------------------- Prediction with ETS and Gradient Descent ----------------------#
#Climate data prediction : Cloud Coverage,Average Temperature,Wind Speed,Wind Direction
drops <- c("Date")
dataCli <- dataClimates[ , !(names(dataClimates) %in% drops)]
#Dataset --> datasetCli

#-------------------------------------------------------------------------------#
#------------------------------- SIGNAL ANALYSIS -------------------------------
# Input : Numerical Sensor Data ( monitoring station of mabegondo, spain (MeteoGalicia))
# Output : Data Abstraction ( Weather Summary & Prediction)

#---------------------- Prediction with ETS and Gradient Descent
#Climate data prediction : Cloud Coverage,Average Temperature,Wind Speed,Wind Direction,Rainfall
#drop date
drops <- c("Date")

predictWeather<-function(dataClimates){
dataCli <- dataClimates[ , !(names(dataClimates) %in% drops)]
	i=1; n=length(dataCli)
	x<-list()
	y<-matrix()
	for(i in i:n){
		#change dataset into time-series dataset (XTS)
		x[[i]] <- xts(dataCli[[i]],dataClimates$Date)
		#forecast with (ETS)
		y[i] <- forecast(x[[i]],h=1)$mean[1]
	}
	i=1
	for(i in i:n){
		names(y)[i]<-paste(colnames(dataCli[i]))
	}
	y<-t(y)
	y<-data.frame(y)
	return(y)
}
y<-predictWeather(dataClimates)
climatePredictionResult <- predictWeather(dataClimates)

#AIR Quality Prediction with ETS linear Regression
drops <- c("Date")
datasetAQ <- dataAQ[ , !(names(dataAQ) %in% drops)]
predictAirQuality<-function(dataAQ){
	
	AQ1 <- xts(datasetAQ,dataAQ$Date)

	i=1; n=length(datasetAQ)
	AQ_ts <- list()
	AQ_predict <- matrix()
	for(i in i:n){
		AQ_ts[[i]] <- xts(datasetAQ[i],dataAQ$Date) ; AQ_predict[i] <- forecast(AQ_ts[[i]],h=1)$mean[1]
	}

	#assign Column name
	i=1
	for(i in i:n){
		names(AQ_predict)[i]<-paste(colnames(datasetAQ[i]))
	}
	AQ_predict<-t(AQ_predict)
	AQ_predict<-data.frame(AQ_predict)
}
AQ_predict<-predictAirQuality(dataAQ)
AQPredictionResult <- data.frame(AQ_predict)

#---------------------- # WEATHER SUMMARY # ----------------------------------#
#----- Statistic Tools (Max, Min, Mean, Total, Trend)------#
#temporary frame for LastMonth
LM <- data.frame()
MBLM <- data.frame()

#-----------------------------
#Filtering "Last Month (LM)" Data
LM <- dataClimates[(NROW(dataClimates)-30):NROW(dataClimates), ]
# MBLM <- subset(MBLM, (Date < (Sys.Date()-30)))
MBLM <- dataClimates[(NROW(dataClimates)-60):(NROW(dataClimates)-30), ]

# ------------------------- SUMMARIZING Last Month
#Defining Max Amount-----------------------------------
i=2
n=length(LM)
col_names <- c(""); max_amt <- c("")
max_index <- c(""); max_date <- c("")
for(i in i:n){
	max_amt[i] <- max(LM[i])
	max_index2 <- as.integer(which(LM[i]==max(LM[i])))
	max_index[i] <- max_index2[1]
	max_index0 <- max_index2[1]
	max_date[i] <- as.character(LM$Date[max_index0])
	col_names[i] <- colnames(LM[i])
}
LMmax_result <- data.frame(col_names, max_date, max_amt)

#Defining Min Amount-----------------------------------
i=2
n=length(LM)
col_names <- c("") ;col_names <- c("")
min_amt <- c(""); min_index <- c("")
min_date <- c("")
for(i in i:n){
	min_amt[i] <- min(LM[i])
	min_index2 <- as.integer(which(LM[i]==min(LM[i])))
	min_index[i] <- min_index2[1]
	min_index0 <- min_index2[1]
	min_date[i] <- as.character(LM$Date[min_index0])
	col_names[i] <- colnames(LM[i])
}
LMmin_result <- data.frame(col_names, min_date, min_amt)

#Defining Mean Amount-----------------------------------
#remove date from LM
xLM <- LM[2:6]
LMmean_result <- colMeans(xLM)

#Defining Sum Amount-----------------------------------
i=2
n=length(LM)
col_names <- c("") ; 
sum_amt <- c("");
for(i in i:n){
	sum_amt[i] <- sum(LM[,i])
	col_names[i] <- colnames(LM[i])
}
LMsum_result <- data.frame(col_names, sum_amt)


# ------------------------- SUMMARIZING MBMBLM
#Defining Max Amount-----------------------------------
i=2
n=length(MBLM)
col_names <- c(""); max_amt <- c("")
max_index <- c(""); max_date <- c("")
for(i in i:n){
	max_amt[i] <- max(MBLM[i])
	max_index2 <- as.integer(which(MBLM[i]==max(MBLM[i])))
	max_index[i] <- max_index2[1]
	max_index0 <- max_index2[1]
	max_date[i] <- as.character(MBLM$Date[max_index0])
	col_names[i] <- colnames(MBLM[i])
}
MBLMmax_result <- data.frame(col_names, max_date, max_amt)

#Defining Min Amount-----------------------------------
i=2
n=length(MBLM)
col_names <- c("") ;col_names <- c("")
min_amt <- c(""); min_index <- c("")
min_date <- c("")
for(i in i:n){
	min_amt[i] <- min(MBLM[i])
	min_index2 <- as.integer(which(MBLM[i]==min(MBLM[i])))
	min_index[i] <- min_index2[1]
	min_index0 <- min_index2[1]
	min_date[i] <- as.character(MBLM$Date[min_index0])
	col_names[i] <- colnames(MBLM[i])
}
MBLMmin_result <- data.frame(col_names, min_date, min_amt)

#Defining Mean Amount-----------------------------------
xMBLM <- MBLM[2:6]
MBLMmean_result <- colMeans(xMBLM)

#Defining Sum Amount-----------------------------------
i=2
n=length(MBLM)
col_names <- c("") ; 
sum_amt <- c("");
for(i in i:n){
	sum_amt[i] <- sum(MBLM[,i])
	col_names[i] <- colnames(MBLM[i])
}
MBLMsum_result <- data.frame(col_names, sum_amt)

#Defining Average for Year
YearMean_result <- colMeans(dataCli)


#--------------------------------------------------------
#--------------- Weather Summary Result
#----------------------------------- Today Weather
n<-nrow(dataClimates)
Today_Rainfall <- dataClimates[n,"Rainfall"]
Today_CloudCoverage <- dataClimates[n,"Cloud.Coverage"]
Today_Temperature <- dataClimates[n,"Average.Temperature"]
Today_WindDirection <- dataClimates[n,"Wind.Direction"]
Today_WindSpeed <- dataClimates[n,"Wind.Speed"]
Today_AirQuality <- dataAQ[n,]

TodaysWeather <- data.frame(Rainfall=Today_Rainfall,CloudCoverage=Today_CloudCoverage,
							Temperature=Today_Temperature, WindDirection=Today_WindDirection,
							WindSpeed=Today_WindSpeed, AirQuality=Today_AirQuality
							)
#----------------------------------- Yesterday Weather
n<-nrow(dataClimates)
Yesterday_Rainfall <- dataClimates[n-1,"Rainfall"]
Yesterday_CloudCoverage <- dataClimates[n-1,"Cloud.Coverage"]
Yesterday_Temperature <- dataClimates[n-1,"Average.Temperature"]
Yesterday_WindDirection <- dataClimates[n-1,"Wind.Direction"]
Yesterday_WindSpeed <- dataClimates[n-1,"Wind.Speed"]
Yesterday_AirQuality <- dataAQ[n-1,]
YesterdaysWeather <- data.frame(Rainfall=Yesterday_Rainfall,CloudCoverage=Yesterday_CloudCoverage,
								Temperature=Yesterday_Temperature, WindDirection=Yesterday_WindDirection,
								WindSpeed=Yesterday_WindSpeed, AirQuality=Yesterday_AirQuality
							)

	cat("\n\n")
	cat("------------ Signal Analysis  Output -----------\n\n")
	cat("------------     Weather Summary     -----------\n\n")
	cat("------------        LAST MONTH       -----------\n\n")
	print(LMmax_result)
	cat("\n")
	print(LMmin_result)
	cat("\n")
	print(LMmean_result)
	cat("\n")
	print(LMsum_result)
	cat("\n")
	cat("------------        LAST 2 MONTH       -----------\n\n")
	print(MBLMmax_result)
	cat("\n")
	print(MBLMmin_result)
	cat("\n")
	print(MBLMmean_result)
	cat("\n")
	print(MBLMsum_result)
	cat("\n")
	cat("------------        AVERAGE YEAR       -----------\n\n")
	print(YearMean_result)
	cat("------------ Today's Weather -----------\n\n")
	TodaysWeatherT<-t(TodaysWeather)
	print(TodaysWeatherT)
	AQPredictionResultT<-t(AQPredictionResult)
	print(AQPredictionResultT)
	cat("\n")
	cat("------------ Prediction With ETS & GD -----------\n\n")
	print(climatePredictionResult)
	print(AQPredictionResult)


	#--------------------------------------------------------------------------------------------------#
	#--------------------------------------------------------------------------------------------------#
	#--------------------------------------------------------------------------------------------------#
	#------------------------------- DATA INTERPRETATION ----------------------------------------------#
	# Output -> Message Inventory ----------------------------------------------#


#-------------------------------INPUT Variable Rainfall

#---------------------- Fuzzy Linguistic Description ---------------------#
#FUnction : Data Interpreter -> translate numerical event into linguistic description  with IF-THEN Rules
#Input -> -Interval : CRISP Set Membership Function (eg. )
#		  -val : Event Value (eg. 24)
#		  -name : Data Name (eg. Rain
#		  -pname : partition name

#menginterpretasikan kualitas udara dengan Polutant Standard Inndex (PSI)
AirQuality_interpreter <- function (dataset){
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
	O3_PSI_value <- (((a2-a1)/(b2-b1))*(O3-b1)+a1)


	PSI_data <- c(PM25_PSI_value,PM10_PSI_value,SO2_PSI_value,CO_PSI_value,O3_PSI_value)
	PSI_value <- as.integer(max(PSI_data))

	print(dataset)
	print(PM10_PSI_value)
	print(PM25_PSI_value)
	print(CO_PSI_value)
	print(O3_PSI_value)
	print(SO2_PSI_value)
	print(SO2)
	print("PSI PSI")
	print(PSI_value)
	if((PSI_value>=0)&&(PSI_value<=50)){
		return("good")
	}
	else if((PSI_value>50)&&(PSI_value<=100)){
		return("admissible")
	}
	else if((PSI_value>100)&&(PSI_value<=250)){
		return("bad")
	}
	else{
		return("hazzardous")
	}

}

Data_Interpreter <- function(interval,val,name,pname){
	n=length(interval) ; i=1;
	for(i in i:n){
	;
	a<-interval[[i]]["a"]
	b<-interval[[i]]["b"]
		if((val>=a)&&(val<=b)){
			result <- pname[[i]]
		}
	}

	return(result)
}

#---- Data Interpreter with Fuzzy Sets ----#
#defining Membership Partition ( Fuzzification Process ) - Trapezoidal
membership_partition <- function(variables,name){
	matrix_graph <- list()
	y=as.matrix(c(1,6,6,1))
	n=length(variables)
	# print(n)
	maxX<-variables[[n]]["d"]
	i=1;
	for(i in i:n){
		title <- paste(name," Membership Function")
		if(i==1){
			# plot(variables[[i]],y,type="l",lwd=1,main=title,xlim=c(0,maxX),yaxt="n",col="red")
		}else{
		 	# lines(variables[[i]],y,lwd=1,col="red")
		}
	}
	return(variables)
}

#check membership----------------------------------------------------
membership_check <- function(partition,v,pname,oname){
	i=1; n=length(partition)
	membership_value <- c(1)
	for(i in i:n){
		a<-partition[[i]]["a"]
		b<-partition[[i]]["b"]
		c<-partition[[i]]["c"]
		d<-partition[[i]]["d"]

		if((v<a)||(v>d)){
			membership_value[i] <- 0
			#print("uhuk")
		}
		if((v>=a)&&(v<=b)){
			membership_value[i] <- (  (v-a) / (b-a)  )
			#print("xyz")
		}
		if((v>b)&&(v<=c)){
			membership_value[i] <- 1
			#print("abc")
		}

		if((v>c)&&(v<=d)){
			membership_value[i] <- (  (d-v) / (d-c)  )
			#print("abc")
		}

		# print(membership_value[[i]])
	}
	#check highest membership result
	i=1; biggest=0; part<-"a"
	for(i in i:n){
		if(is.nan(membership_value[i])){
			membership_value[i] <- 0
		}
		if(biggest<=membership_value[i]){
		biggest <- membership_value[i]
		part<-pname[i]
		}
	}
	part <- paste(part,oname)
	mval <- as.matrix(membership_value)
	return (part)
}
#--------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#------------------------------- PREDICTION INTERPRETATION -------------------------------
#-------------------------------INPUT Variable Rainfall
if(climatePredictionResult["Rainfall"]==0){
InterpretationResult_rainfall <- "no rain"	
}else{
Rainfall_partition <- c("light","moderate","heavy","intense","torential")
Rainfall_interval <- list(light=c(a=0,b=0,c=2.5,d=3.75),moderate=c(a=2.5,b=2.75,c=7.5,d=11.25),
						  heavy=c(a=7.5,b=11.25,c=15,d=30), intense=c(a=15,b=30,c=50,d=60),
						  torential=c(a=50,b=60,c=70,d=80))
Rainfall_interval <- membership_partition(Rainfall_interval,"Rainfall")
InterpretationResult_rainfall <- membership_check(Rainfall_interval,as.double(climatePredictionResult["Rainfall"]),Rainfall_partition,"rain")
}
# #-------------------------------INPUT Temperature
Temperature_partition <- c("very cold.","cold.","warm.","hot.","very hot.")
Temperature_interval <- list(very_cold=c(a=0,b=0,c=5,d=10),cold=c(a=5,b=10,c=15,d=20),
						  warm=c(a=15,b=20,c=25,d=30),
						  hot=c(a=25,b=30,c=35,d=40),very_hot=c(a=35,b=40,c=45,d=50))
Temperature_interval <- membership_partition(Temperature_interval,"Temperature")
InterpretationResult_temperature <- membership_check(Temperature_interval,as.double(climatePredictionResult["Average.Temperature"]),Temperature_partition," ")


# #-------------------------------INPUT Variable Cloud.Coverage
# # 0-10 clear / sunny
# # 10-20 fair (often saved for high wispy cirrus)
# # 20-30 mostly sunny
# # 30-50 partly cloudy
# # 50-70 mostly cloudy
# # 70-80 broken
# # 80-100 cloudy / overcast
CloudCoverage_partition <- c("clear","foggy","mostly sunny","partly cloudy","mostly cloudy","broken","overcast")
CloudCoverage_interval <- list(clear=c(a=0,b=10),foggy=c(a=10,b=20),
						  mostly_sunny=c(a=20,b=30), partly_cloudy=c(a=30,b=50),
						  mostly_cloudy=c(a=50,b=70), broken=c(a=70,b=80),ovrecast=c(a=80,b=100))

InterpretationResult_cloudCoverage <- Data_Interpreter(CloudCoverage_interval,
									  as.double(climatePredictionResult["Cloud.Coverage"]),
									  "sky ",CloudCoverage_partition)


# #-------------------------------INPUT Wind Direction

WindDirection_partition <- c("from the north.","from the north north east.","from the north east.",
								"from the east north east.","from the east.","from the east south east.",
								"from the south east.","from the south south east.","from the south.",
								"from the south south west.","from the south west.","from the west south west.",
								"from the west.","from the west north west.","from the north west.",
								"from the north north west.", "from the north."
								)
WindDirection_interval <- list(
								N=c(a=0,b=11.25),NNE=c(a=11.25,b=33.75), NE=c(a=33.75,b=56.25),
								ENE=c(a=56.25,b=78.25),E=c(a=78.75,b=101.5), ESE=c(a=101.5,b=123.75),
								SE=c(a=123.75,b=146.25),SSE=c(a=146.25,b=168.75), S=c(a=168.75,b=191.25),
								SSW=c(a=191.25,b=213.75),SW=c(a=213.75,b=236.25), WSW=c(a=236.25,b=258.75),
								W=c(a=258.75,b=281.25),WNW=c(a=281.25,b=303.75), NW=c(a=303.75,b=326.25),
								NNW=c(a=326.25,b=348.75),FN=c(a=348.75,b=360)
								)
InterpretationResult_windDirection <- Data_Interpreter(WindDirection_interval,
									  as.double(climatePredictionResult["Wind.Direction"]),
									  "",WindDirection_partition)

# #-------------------------------INPUT Wind Speed

WindSpeed_partition <- c("calm","light air","light breeze",
						"gentle breeze","moderate breeze","fresh breeze",
						"strong breeze","near gale","gale","strong gale",
						"storm", "violent storm", "Hurrricane"
						 )
WindSpeed_interval <- list(calm=c(a=0,b=2),light_air=c(a=2,b=5),
						  light_breeze=c(a=5,b=11), gentle_breeze=c(a=11,b=19),
						  moderate_breeze=c(a=19,b=29), fresh_breeze=c(a=29,b=39),
						  strong_breeze=c(a=39,b=50),near_gale=c(a=50,b=61),
						  gale=c(a=61,b=74), strong_gale=c(a=74,b=87),
						  storm=c(a=87,b=102), violent_storm=c(a=102,b=118),
						  hurricane=c(a=118,b=130)
						  )
InterpretationResult_windSpeed <- Data_Interpreter(
									WindSpeed_interval,
									as.double(climatePredictionResult["Wind.Speed"]),
								  	"wind",
								  	WindSpeed_partition)
# # #-------------------------------------------------------------------------------------------------------------
# # #---------------------- AQ
InterpretationResult_airQuality <- AirQuality_interpreter(AQPredictionResult)


#interpretation of Today's Rainfall
if(Today_Rainfall==0){
TodaysInterpretationResult_rainfall <- "no rain"	
}else{
 TodaysInterpretationResult_rainfall <- membership_check(Rainfall_interval,Today_Rainfall,Rainfall_partition,"rain")
}

TodaysInterpretationResult_cloudCoverage <- Data_Interpreter(CloudCoverage_interval,Today_CloudCoverage,"sky",CloudCoverage_partition)
TodaysInterpretationResult_temperature <- membership_check(Temperature_interval,Today_Temperature,Temperature_partition," ")
TodaysInterpretationResult_windDirection <- Data_Interpreter(WindDirection_interval,Today_WindDirection," ",WindDirection_partition)
TodaysInterpretationResult_windSpeed <- Data_Interpreter(WindSpeed_interval,Today_WindSpeed,"wind",WindSpeed_partition)
TodaysInterpretationResult_airQuality <- AirQuality_interpreter(Today_AirQuality)

if(Yesterday_Rainfall==0){
YesterdayInterpretationResult_rainfall <- "no rain"	
}else{
YesterdayInterpretationResult_rainfall <- membership_check(Rainfall_interval,Yesterday_Rainfall,Rainfall_partition,"rain")
}

YesterdayInterpretationResult_cloudCoverage <- Data_Interpreter(CloudCoverage_interval,Yesterday_CloudCoverage,"sky",CloudCoverage_partition)
YesterdayInterpretationResult_temperature <- membership_check(Temperature_interval,Yesterday_Temperature,Temperature_partition," ")
YesterdayInterpretationResult_windDirection <- Data_Interpreter(WindDirection_interval,Yesterday_WindDirection," ",WindDirection_partition)
YesterdayInterpretationResult_windSpeed <- Data_Interpreter(WindSpeed_interval,Yesterday_WindSpeed,"wind",WindSpeed_partition)
YesterdayInterpretationResult_airQuality <- AirQuality_interpreter(Yesterday_AirQuality)

AQ_seq <- c(YesterdayInterpretationResult_airQuality,TodaysInterpretationResult_airQuality,InterpretationResult_airQuality)
# #----------------------------------------------------------------------------------n----------------#
# #--------------------------------------------------------------------------------------------------#
# #--------------------------------------------------------------------------------------------------#

	cat("------------ Data Interpretation Output -----------\n\n")
	cat("------------ Yesterday -----------\n\n")
	cat("      Yesterday Rainfall          : ",YesterdayInterpretationResult_rainfall,"\n\n")
	cat("      Yesterday Cloud Coverage    : ",YesterdayInterpretationResult_cloudCoverage,"\n\n")
	cat("      Yesterday Temperature (AVG) : ",YesterdayInterpretationResult_temperature,"\n\n")
	cat("      Yesterday Wind Direction    : ",YesterdayInterpretationResult_windDirection,"\n\n")
	cat("      Yesterday Wind Speed        : ",YesterdayInterpretationResult_windSpeed,"\n\n")
	cat("      Yesterday Air Quality       : ",YesterdayInterpretationResult_airQuality,"\n\n")
	cat("------------ Today-----------\n\n")
	cat("      Today's Rainfall            : ",TodaysInterpretationResult_rainfall,"\n\n")
	cat("      Today's Cloud Coverage      : ",TodaysInterpretationResult_cloudCoverage,"\n\n")
	cat("      Today's Temperature (AVG)   : ",TodaysInterpretationResult_temperature,"\n\n")
	cat("      Today's Wind Direction      : ",TodaysInterpretationResult_windDirection,"\n\n")
	cat("      Today's Wind Speed          : ",TodaysInterpretationResult_windSpeed,"\n\n")
	cat("      Today's Air Quality         : ",TodaysInterpretationResult_airQuality,"\n\n")
	cat("------------ Predicted -----------\n\n")
	cat("      Predicted Rainfall          : ",InterpretationResult_rainfall,"\n\n")
	cat("      Predicted Cloud Coverage    : ",InterpretationResult_cloudCoverage,"\n\n")
	cat("      Predicted Temperature (AVG) : ",InterpretationResult_temperature,"\n\n")
	cat("      Predicted Wind Direction    : ",InterpretationResult_windDirection,"\n\n")
	cat("      Predicted Wind Speed        : ",InterpretationResult_windSpeed,"\n\n")
	cat("      Predicted Air Quality       : ",InterpretationResult_airQuality,"\n\n")

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#


#----------------------------- Prediction Document Planning ----------------------------------#
# Output : Routine Message 			 -> Rainfall, Cloud & Air Quality 
# 		   Significant Event Message -> Wind Speed & Wind Direction
# Document Schema :
#		   Prediction Summary -> Sky State -> {Rain State, Cloud State}
#							  -> Temperature
#							  -> Wind State -> {Wind Speed, Wind Direction}
#							  -> Air Quality State
# Simple Realiser
substrRight <- function(x, n){
 	substr(x, nchar(x)-n+1, nchar(x))
	}

ordinal_indicator <- function(num){
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

Coldest_day <- function(LMmin_result,LMmean_result,MBLMmean_result){
x<-as.double(LMmean_result["Average.Temperature"])
y<-as.double(MBLMmean_result["Average.Temperature"])
if(x>y){
	trend <- "increased"
	conj <- "but"
}
else{
	trend <- "decreased"
	conj <- "and"
}
date <- as.character(LMmin_result[3,2])
value <- LMmin_result[3,3]
	oi<-ordinal_indicator(date)
	dt <- substrRight(date,2)
	msg<-paste(
		"Average temperature was",trend,conj,
		dt,oi," was the coldest day of the month with ",value," celcius degree temperature.")
}

#Simple Realizer

MonthlyTempMessage_function <- function(YearMean_average, MonthMean_average){
	yearmean <- as.double(YearMean_average["Average.Temperature"])
	monthmean <- as.double(MonthMean_average["Average.Temperature"])
	if(yearmean<monthmean){
		message<- c("cooler",0)
	}
	else if(yearmean==monthmean){
		message<- c("stable",1)
	}
	else if(yearmean>monthmean){
		message<- c("warmer",2)
	}
	return(message)
}

MonthlyTempMsg <- MonthlyTempMessage_function(YearMean_result, LMmean_result)

MonthlyRainfallMessage_function <- function(YearMean_average, MonthMean_average){
	yearmean <- as.double(YearMean_average["Rainfall"])
	monthmean <- as.double(MonthMean_average["Rainfall"])
	if(yearmean<monthmean){
		message<- c("drier",2)
	}
	else if(yearmean==monthmean){
		message<- c("same",1)
	}
	else if(yearmean>monthmean){
		message<- c("wetter",0)
	}
	return(message)
}

MonthlyRainfallMsg <- MonthlyRainfallMessage_function(YearMean_result, LMmean_result)

RainyDaysMessage_function <- function(LM){
	n<-nrow(LM); i<-1; number_rain<-0; x<-LM["Rainfall"]; msg<-""
	for(i in i:n){
		if(x[i,1]>0){
			number_rain <- number_rain + 1
		}
	}
	if(number_rain<=5){
		msg<-c("low number of rain",0)
	}
	else if(number_rain>=6 && number_rain <=15){
		msg<-c("average number of rain",0)
	}
	else{
		msg<-c("high number of rain",2)
	}
	return(msg)
}
RainyDaysMsg <- RainyDaysMessage_function(LM)
 
substrRight <- function(x, n){
	  substr(x, nchar(x)-n+1, nchar(x))
	}

RainSoFarMessage_function <- function(LM,MonthMean_average){
	LM <- LM[(NROW(LM)-7):NROW(LM), ]
	n<-nrow(LM); i<-1; number_rain<-0; x<-as.matrix(LM["Rainfall"]); msg<-"";
	weeklymean<-mean(x)
	monthlymean <- as.double(MonthMean_average["Rainfall"])
	if(weeklymean>monthlymean){
		msg<-c("well above the average",2)
	}
	else{
		msg<-c("well below the average",0)
	}
	return(msg)
}
RainSoFarMessage <- RainSoFarMessage_function(LM,LMmean_result)

RainSpellMessage<-function(LM){
	x<-as.matrix(LM["Rainfall"])
	n<-length(x);i<-1;j<-0;k<-1;l<-0;status=0;
	seq<-matrix("0",nrow=n,ncol=3)
	for(i in i:n){
		if(x[i]==0){
			status=0
			if(i!=1){
				if(x[i-1]>0){
				out_date <- as.character(LM[i-1,"Date"])
				seq[j,3] <- out_date
				}
			}
		}
		else if((x[i]>0)&&(status==0)){
			status=1
			j<-j+1
			seq[j,1] <- as.integer(seq[j,1]) + 1
			in_date <- as.character(LM[i,"Date"])
			seq[j,2] <- in_date
		}
		else if((x[i]>0)&&(status==1)){
			seq[j,1] <- as.integer(seq[j,1]) +1
		}
		if(i==n){
			if((x[i]!=0)&&(status==1)){
				out_date <- as.character(LM[i,"Date"])
				seq[j,3] <- out_date
			}
		}
		print(j)
	}
	print("AWWW")
	print(seq)
	msg<-0

	max_index<-which.max(seq[,1])
	in_spell<-substrRight(seq[max_index,2],2)	
	out_spell<-substrRight(seq[max_index,3],2)
	in_mnth<-substr(seq[max_index,2],6,7)
	out_mnth<-substr(seq[max_index,2],6,7)
	in_ordinal <- ordinal_indicator(in_spell)
	out_ordinal <- ordinal_indicator(out_spell)
	if(max_index==1){
		return("")
	}
	if(in_mnth==out_mnth){
		max_spell<-paste(max(seq[,1])," days from ",in_spell,in_ordinal,
					 " to ",out_spell,out_ordinal," ",sep="")
	}
	else{
		in_mnth <- month.abb[as.double(in_mnth)]
		out_mnth <- month.abb[as.double(out_mnth)]
		max_spell<-paste(max(seq[,1])," days from ",in_spell,in_ordinal,in_mnth,
					 " to ",out_mnth,out_spell,out_ordinal," ",sep="")
	}
	msg<-max_spell
	return(msg)
}

RainSpellMsg <- RainSpellMessage(LM)



RainExtremeMessage_function <- function(LM){
	blewmsg <- as.matrix(read.table(file="wordbank/RainExtreme_lex.csv", sep=",", header=TRUE))
	n=length(blewmsg); i=1
	random_value <- as.integer(runif(1,1,n+1))
	verb <- blewmsg[random_value]


	n<-nrow(LM); i<-1; j<-0; number_rain<-0; x<-as.matrix(LM["Rainfall"]); msg<-""
	z<-max(x)
	status<-""
	if(z>0){

	status <- membership_check(Rainfall_interval,z,Rainfall_partition,"rain")
		if((status=="no rain")||(status=="light rain")||(status=="moderate rain")){
			return("x")
		}
	}
	# print(status)
	time_happened<-matrix()
	for(i in i:n){
		y <- as.double(LM[i,"Rainfall"])
		# status2 <- membership_check(Rainfall_interval,as.double(LM[i,"Rainfall"]),Rainfall_partition,"rain")
		if(y>0){
			status2 <- membership_check(Rainfall_interval,y,Rainfall_partition,"rain")
			if(status==status2){
				j<-j+1
				time_happened[j]<- substrRight(as.character(LM[i,"Date"]),2)			
			}
		}else{

		}
	}
	if(j==1){
		aux <- "was"
		verb <- paste(verb,"in ")
		}else{
		aux <- "were"
		verb <- paste(verb,"in ")
		}

	msg<-paste(status,aux,verb)
	i=1; 
	for(i in i:j){
		if(j==1){
				oi <- ordinal_indicator(time_happened[i])
				th <- paste(time_happened[i],oi,sep="")
				msg<- paste(" ",msg,th,".",sep="")
		}
		else{
			if((i!=j)&&(i!=j-1)){
				oi <- ordinal_indicator(time_happened[i])
				th <- paste(time_happened[i],oi,sep="")
				msg<- paste(" ",msg,th,", ",sep="")
			}
			else if(i==j-1){
				oi <- ordinal_indicator(time_happened[i])
				th <- paste(time_happened[i],oi,sep="")
				msg<- paste(msg," ",th,sep="")
			}
			else{
				oi <- ordinal_indicator(time_happened[i])
				th <- paste(time_happened[i],oi,sep="")
				msg<-paste(msg," and ",th,".",sep="")
			}
		}
		
	}
	if((status=="light rain")||(status=="moderate rain")){
			return("false")
		}
	else{
	return(msg)
	}
}
RainExtremeMsg <- RainExtremeMessage_function(LM)


MonthlyWindMessage<-function(data){
	x<-as.double(data["Wind.Speed"])
	msg <- Data_Interpreter(WindSpeed_interval,
							 x, "",WindSpeed_partition)
	return(msg)
}
MonthlyWindMsg<-MonthlyWindMessage(LMmean_result)

WindExtremeMessage_function <- function(LM){
	blewmsg <- as.matrix(read.table(file="wordbank/WindExtreme_lex.csv", sep=",", header=TRUE))
	n=length(blewmsg); i=1
	random_value <- as.integer(runif(1,1,n+1))
	verb <- blewmsg[random_value]
	n<-nrow(LM); i<-1; j<-0; number_rain<-0; x<-as.matrix(LM["Wind.Speed"]); msg<-""
	z<-max(x)
	if(z>0){
	status <- Data_Interpreter(WindSpeed_interval,z,"wind",WindSpeed_partition)
	}
	# print(status)
	time_happened<-matrix()
	for(i in i:n){
		y <- as.double(LM[i,"Wind.Speed"])
		if(y>0){
			status2 <- Data_Interpreter(WindSpeed_interval,y,"wind",WindSpeed_partition)
			if(status==status2){
				print(status2)
				j<-j+1
				ss<-substrRight(as.character(LM[i,"Date"]),2)
				time_happened[j]<-i
				# print(time_happened[j])
			}
		}else{

		}
	}
	i=1;
	if(j==1){
		aux <- "was"
		verb <- paste(verb,"in ")
		}else{
		aux <- "were"
		verb <- paste(verb,"in ")
		}
	msg<-paste(status,aux,verb)
	i=1; 
	for(i in i:j){
		if(j==1){
				oi <- ordinal_indicator(time_happened[i])
				th <- paste(time_happened[i],oi,sep="")
				msg<- paste(" ",msg,th,".",sep="")
		}
		else{
			if(i!=j){
				oi <- ordinal_indicator(time_happened[i])
				th <- paste(time_happened[i],oi,sep="")
				msg<- paste(" ",msg,th,", ",sep="")
			}
			else if(i==j-1){
				oi <- ordinal_indicator(time_happened[i])
				th <- paste(time_happened[i],oi,sep="")
				msg<- paste(msg," ",th,sep="")
			}
			else{
				oi <- ordinal_indicator(time_happened[i])
				th <- paste(time_happened[i],oi,sep="")
				msg<-paste(msg," and ",th,".",sep="")
			}
		}
	}
	if((status=="calm")||(status=="light air")||(status=="light breeze")||
		(status=="gentle breeze")||(status=="moderate breeze")||(status=="fresh breeze")){
			return("false")
		}
	else{
	return(msg)
	}
}
WindExtremeMsg <- WindExtremeMessage_function(LM)

MonthlyAQMessage_function <- function(AQData){
	MonthAQ <- AQData[(NROW(AQData)-30):NROW(AQData), ]
	monthmean <- colMeans(MonthAQ)
	msg<-AirQuality_interpreter(monthmean)
	return(msg)
}

MonthlyAQMsg <- MonthlyAQMessage_function(datasetAQ)



# AQExtremeMsg <- AQExtremeMessage_function(datasetAQ)

#--------------------------------------------------------------------------------------------------#
#--------------------------------  Linguistic Realization    ---------------------------------------#
# --> untuk pesan yang dibandingkan


#----------------------------- Microplanning for Prediction ------------------------------------------# 

# Lexicalisation proses

# Source = Ramos
# Function

LD_Compare <- function (data, value){
	#map data-set to Index-set
	i=1; n=length(data); index_data<-matrix();
	for(i in i:n){
		j=1; m=length(AQ_val)
		for(j in j:m){
			x<-colnames(value[j])
			#cat("x : ",x," data: ",data[i],"\n")
			if(data[i]==x){
				index_data[i]<-value[j]
			}
		}
	}
	#Compute Index Variation
	i=1; n=length(data); IV<-c(0,0);
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
	x<-TrendDesc_template(IVL)
	return(x)
}

AQ_Intro <- function(){
	intro_AQ <- as.matrix(read.table(file="wordbank/AQ_referring_expression.csv", sep=",", header=TRUE))
	n=length(intro_AQ); i=1
	random_value <- as.integer(runif(1,1,n+1))
	return(intro_AQ[random_value])
}

Prediction_Intro <- function(){
	intro_PR <- as.matrix(read.table(file="wordbank/PR_referring_expression.csv", sep=",", header=TRUE))
	n=length(intro_PR); i=1
	random_value <- as.integer(runif(1,1,n+1))
	return(intro_PR[random_value])
}

Temperature_Intro <- function(){
	intro_Temp <- as.matrix(read.table(file="wordbank/Temp_intro.csv", sep=",", header=TRUE))
	n=length(intro_Temp); i=1
	random_value <- as.integer(runif(1,1,n+1))
	return(intro_Temp[random_value])
}


TrendDesc_template <- function (IVL,data){
	if((IVL[1]=="0")&&(IVL[2]=="0")){
		TrendDesc <- change_word_bank_AQ("stable")
	}
	if(((IVL[1]=="+")&&(IVL[2]=="-"))||((IVL[1]=="-")&&(IVL[2]=="+"))){
		TrendDesc <- change_word_bank_AQ("mediumChange")
	}
	if(((IVL[1]=="+")&&(IVL[2]=="0"))||((IVL[1]=="-")&&(IVL[2]=="0"))){
		TrendDesc <- change_word_bank_AQ("startChange")
	}
	if(((IVL[1]=="0")&&(IVL[2]=="+"))||((IVL[1]=="0")&&(IVL[2]=="-"))){
		TrendDesc <- change_word_bank_AQ("endChange")
	}
	if(((IVL[1]=="+")&&(IVL[2]=="+"))||((IVL[1]=="-")&&(IVL[2]=="-"))){
		TrendDesc <- change_word_bank_AQ("progressiveChange")
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

#Function Sky State Aggregation with Simple Conjunction
Sky_Agg <- function (rain,cloud){
	#Assign Rule for Contrast Value for each partition of rain state
	if(rain=="no rain"||rain=="light rain"){
		Contrast1=0
	}
	else if(rain=="moderate rain"||rain=="heavy rain" ||
			rain=="intense rain" || rain=="torential rain"
			){
		Contrast1=1
	}
	#Assign Rule for Contrast Value for each partition of cloud state
	if(cloud=="clear"||cloud=="foggy"||cloud=="mostly sunny"){
		Contrast2=0
	}
	else if(cloud=="partly cloudy"||cloud=="mostly cloudy"||cloud=="broken"
			|| cloud=="overcast"
			){
		Contrast2=1
	}

	if(Contrast1==Contrast2){

		Conjunction<-"covered with"
	}else{

		Conjunction<- "although its covered by"

	}

	phrase <- paste(rain,Conjunction,cloud,"sky.")
	return(phrase)
}

Wind_relation <- function(status, direction){
x<-("")
	x<- paste("the wind will blow",status,direction) 
	if((status=="calm")||(status=="light air")||(status=="light breeze")||
		(status=="gentle breeze")||(status=="moderate breeze")||(status=="fresh breeze")){
			return("")
		}
	else{
	return(x)
	}
}

TrendDesc_2 <- function(var1,var2){
	
	print("WOOOY")
	print(var1)
	print(var2)
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

#----------------------------- Microplanning for Weather Summary ------------------------------------------# 

Contrast_lexicalisation1 <- function(msg1,msg2){
	if(msg1[2]==msg2[2]){
		return("and")
	}else{
		return("but")
	}
}

MonthlyIntro_lex<-function(){
	MI <- as.matrix(read.table(file="wordbank/Monthly_referring_expression.csv", sep=",", header=TRUE))
	n=length(MI); i=1
	random_value <- as.integer(runif(1,1,n+1))
	return(MI[random_value])
}
MonthlyIntro <- MonthlyIntro_lex()

MonthlyMsg1_aggregation<-function(msg1,msg2){
	#aggreagation with simple conjunction
	conj <- Contrast_lexicalisation1(msg1,msg2)
	print("asdasd")
	print(msg2[2])
	print(msg1[2])
	msg<-paste("was",msg1[1],conj,msg2[1],"than average.")
	return(msg)
}

MonthlyMsg1 <- MonthlyMsg1_aggregation(MonthlyTempMsg,MonthlyRainfallMsg)

MonthlyMsg2_aggregation<-function(msg1,msg2){
	#aggreagation with simple conjunction
	if(msg1[2]!=msg2[2]){
		contrast<-""
	}else{
		contrast<-"accordingly"
	}
	msg<-paste("With",msg1[1],"days,",contrast,"the total rain so far is",msg2[1],".")
	return(msg)
}
MonthlyMsg2 <- MonthlyMsg2_aggregation(RainyDaysMsg,RainSoFarMessage)


MonthlyMsg3_aggregation<-function(msg1,msg2){
	if(msg2=="x"){
	msg<-paste(" There was rain on everyday for ",msg1,". ",sep="")
	}
	else if(msg1==""){
	msg<-msg2
	}
	else{
	msg<-paste("There was rain on everyday for",msg1,"and",msg2)
	}
	return(msg)
}
MonthlyMsg3 <- MonthlyMsg3_aggregation(RainSpellMsg,RainExtremeMsg)

MonthlyMsg4_aggregation<-function(msg1,msg2){
	if(msg2=="false"){
	msg<-paste("The wind for the month was",msg1,"in average.")
	}
	else{
	msg<-paste("The wind for the month was",msg1,"in average, but",msg2)
	}
	return(msg)
}
MonthlyMsg4 <- MonthlyMsg4_aggregation(MonthlyWindMsg,WindExtremeMsg)

MonthlyMsg5_aggregation<-function(msg1,msg2){
	if(msg2!=""){
	msg<-paste("Average air quality was",msg1,", although",msg2)
	}
	else{
	msg<-paste("Average air quality was ",msg1,".",sep="")
	}
}
MonthlyMsg5 <- MonthlyMsg5_aggregation(MonthlyAQMsg,"")

MonthlyMsg6 <- Coldest_day(LMmin_result,LMmean_result,MBLMmean_result)

#-----------------------------------------------------------------------------------------------------------#

AQ_val <- data.frame(good=0,admissible=1,bad=2,hazardous=3)
TrendDesc_AQ <- LD_Compare(AQ_seq,AQ_val)
TrendDesc_Temperature <- TrendDesc_2(as.double(TodaysWeather["Temperature"]), as.double(y[,"Average.Temperature"]))
Intro_AQ <- AQ_Intro()

Wind_Description<-Wind_relation(InterpretationResult_windSpeed, InterpretationResult_windDirection)
Temperature_Description<-paste(TrendDesc_Temperature, InterpretationResult_temperature)
Temperature_State<-Temperature_Description
#----------------------------------------------------------#
AQ_Description <- paste(Intro_AQ,TrendDesc_AQ,AQ_seq[3],".")

# print(AQ_Description)
# print(Temperature_Description)
# print(Wind_Description)

Structure_Realization_predict <- function(InterpretationResult_rainfall, InterpretationResult_cloudCoverage){
Rain_State <- InterpretationResult_rainfall
Cloud_State <- InterpretationResult_cloudCoverage
Sky_State <- Sky_Agg(Rain_State,Cloud_State)
Sky_Intro <- Prediction_Intro()
Sky_Sentence <- paste(Sky_Intro,Sky_State)
Temperature_Intro <- Temperature_Intro()
Temperature_Sentence <- paste(Temperature_Intro, Temperature_State)
AQ_Sentence <-AQ_Description
Wind_Sentence <- Wind_Description
Prediction_Result <- paste(Sky_Sentence,Temperature_Sentence, AQ_Sentence,Wind_Sentence)
return(Prediction_Result)
}
Prediction_Result<-Structure_Realization_predict(InterpretationResult_rainfall,InterpretationResult_cloudCoverage)

MonthlyMsg<- paste(MonthlyIntro,MonthlyMsg1,MonthlyMsg2,MonthlyMsg3,MonthlyMsg4,MonthlyMsg5,MonthlyMsg6)

print(strwrap(Prediction_Result, width=60))
cat("\n")
print(strwrap(MonthlyMsg,width=60))