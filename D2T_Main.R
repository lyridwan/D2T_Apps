setwd("~/GitHub/D2T_Apps")
# INITIALIZING
source("D2T_Machine.R", local = TRUE)
# source("UnspecificHandling.R", local = TRUE)
# source("SignalAnalysis.R", local = TRUE)
# source("DataInterpretation.R", local = TRUE)
# source("DocumentPlanning.R", local = TRUE)
# source("Microplanning.R", local = TRUE)

#-----------------------
# GENERAL DATA HANDLER |
#-----------------------
# Force read, with default parameter v2,v3,v4,etc if there's no header available

filename <- "DataTest#1"
dataset <- as.data.frame(fread(file=paste0("DatasetsExperiment/",filename,".csv")))
colnames(dataset)[1] <- "DateTime"
dataTitle <- readChar("Config/datatitle.csv", file.info("Config/datatitle.csv")$size)

# Dataset with datetime Column dropped
datasetWithoutDate <- dataset[ , colnames(dataset) != "DateTime"]

# Parameter Header
columnName <- colnames(datasetWithoutDate)

#-----------------------
#   PARAMETER CONFIG   |
#-----------------------
mainConfig <- ReadConfig()


#-----------------------
#   SPLITTING DATASET  | 1. Numerical Dataset
#-----------------------
# Listing all categorical parameter
categoricalType <- c("categorical", "factors", "character")
catColName <- mainConfig[mainConfig$Type %in% categoricalType, "ColName"]

# Numerical Dataset initialization
datasetNumerical <- dataset[, !names(dataset) %in% catColName]
datasetNumericalWithoutDate <- datasetNumerical[ , colnames(datasetNumerical) != "DateTime"]
columnNameNumerical <- colnames(datasetNumericalWithoutDate)

#-----------------------
#   SPLITTING DATASET  | 2. Categorical Dataset
#-----------------------
if(length(catColName) != 0){
  if(length(catColName) == 1){
    datasetCategoricalWithoutDate <- dataset[, catColName]
    datasetCategorical <- cbind(dataset["DateTime"], datasetCategoricalWithoutDate)
    colnames(datasetCategorical)[2] <- catColName
  }else{
    datasetCategoricalWithoutDate <- dataset[, colnames(dataset) %in% catColName]
    datasetCategorical <- cbind(dataset["DateTime"], datasetCategoricalWithoutDate)
  }
}

#-----------------------
#  DATE TIME INTERVAL  | 
#-----------------------
# Dataset Interval
datasetIntervalValue <- DateInterval(dataset[2,"DateTime"], dataset[1,"DateTime"])


# SYGNAL ANALYSIS: 1.Data Summarizing
# -------------Begin------------- 

# Statistical Resume
statisticalResume <- StatisticalAnalysis(datasetNumerical)

i <- 1
vectorTrendAnalysisResult <- c()
for(i in i:length(datasetNumericalWithoutDate)){
  vectorColumn <- datasetNumericalWithoutDate[[i]]
  minValue <- as.numeric(as.character(statisticalResume$MinValue)[i])
  maxValue <- as.numeric(as.character(statisticalResume$MaxValue)[i])
  vectorTrendAnalysisResult[i] <- TrendAnalysis(1, vectorColumn, minValue, maxValue)
}

#merging main analysis DF with trend column
statisticalResume$Trend <- vectorTrendAnalysisResult

# ------------End------------ 


# SYGNAL ANALYSIS: 2.Extreme Event
# ------------Begin------------ 

dfExtremeGrowth <- ResumeEventExtreme(datasetNumericalWithoutDate, statisticalResume, "Growth")
dfExtremeDecay <- ResumeEventExtreme(datasetNumericalWithoutDate, statisticalResume, "Decay")

dfExtremeEvent <- cbind(columnNameNumerical, dfExtremeGrowth, dfExtremeDecay)
# ------------End------------ 

# SYGNAL ANALYSIS: 3.Repeated Event
# ------------Begin------------ 

# Function: ResumeRepeatedAnalysis()
# Purpose: Analyzing Repeated Event 
# Result variable: listRepeatedAnalysisResult

i <- 1
listRepeatedAnalysisResult <- list()
for(i in i:length(datasetNumericalWithoutDate)){
  vectorColumn <- datasetNumericalWithoutDate[[i]]
  listRepeatedAnalysisResult[[i]] <- ResumeRepeatedAnalysis(vectorColumn)
}
# ------------End------------ 

# SYGNAL ANALYSIS: 4.Predicting
# ------------Begin------------ 

datasetPredicted <- PredictDataset(datasetNumerical)
# ------------End------------ 

# SYGNAL ANALYSIS: 5.Motif Discovery
# ------------Begin------------ 

MDinterpreterResult <- MotifDiscoveryInterpreter(datasetWithoutDate, datasetIntervalValue)
# ------------End------------ 


# SYGNAL ANALYSIS: 6.Correlation
# ------------Begin------------ 

correlationResult <- CorrelationAnalysis(datasetNumericalWithoutDate)
# ------------End------------ 




# DATA INTERPRETATION: 0.Preparation
# ------------Begin------------ 
# row [N] data, now
datasetNow <- datasetNumerical[nrow(datasetNumerical), !colnames(datasetNumerical) == "DateTime"]

# row[N-1] data, before now
datasetLast <- datasetNumerical[nrow(datasetNumerical)-1, !colnames(datasetNumerical) == "DateTime"]

# row[N-2] data, before now
dataset2Last <- datasetNumerical[nrow(datasetNumerical)-2, !colnames(datasetNumerical) == "DateTime"]

# Average Resume
averageResume <- as.data.frame.list(colMeans(datasetNumerical[, !colnames(datasetNumerical) == "DateTime"]))
# ------------End------------ 

# SYGNAL ANALYSIS: 2.Extreme Event
# ------------Begin------------ 
comparsionResult <- ComparsionAnalysis(datasetNow, datasetNumerical, datasetIntervalValue)
# ------------End------------ 


# DATA INTERPRETATION: 1. Inpterpretating data
# ------------Begin------------ 

interpreterNow <- DataInterpreter(datasetNow,statisticalResume)
interpreterLast <- DataInterpreter(datasetLast,statisticalResume)
interpreter2Last <- DataInterpreter(dataset2Last, statisticalResume)
interpreterResume <- DataInterpreter(averageResume,statisticalResume)
interpreterPredict <- DataInterpreter(datasetPredicted,statisticalResume)
# ------------End------------

# RESUME LEXICAL PROCESS
i <- 1
vectorTrendDescriptionAnalysis <- c()
for(i in i:length(datasetNumericalWithoutDate)){
  last2Index <- interpreter2Last$InterpreterIndex[i]
  lastIndex <- interpreterLast$InterpreterIndex[i]
  nowIndex <- interpreterNow$InterpreterIndex[i]
  
  vectorSequenceIndex <- c(last2Index, lastIndex, nowIndex)
  # print(vectorSequenceIndex)
  vectorTrendDescriptionAnalysis[i] <- LD_Compare(vectorSequenceIndex)
}



# STRUCTURE REALISATION
resumeIntro <- ReadResumeIntro(dataset["DateTime"], columnName, dataTitle)
trendIntro <- ReadIntro(type="Trend")
resumeTrend <- paste0(trendIntro," ",ResumeTrend(statisticalResume))
resumeComparsion <- ComparsionMessage(datasetNow, comparsionResult, datasetIntervalValue)
resumeRepeated <- RepeatedEventDocPlanning(listRepeatedAnalysisResult)

MDdocPlanResult <- MotifDiscoveryDocPlan(MDinterpreterResult)
resumeMotifDiscovery <- MotifDiscoveryMicroPlan(MDdocPlanResult, MDinterpreterResult)

resumeCorrelationRoutine <- CorrelationRoutineMessage(correlationResult)
resumeCorrelationSignificant <- CorrelationSignificantMessage(correlationResult)
resumeCorrelation <- paste(resumeCorrelationRoutine, resumeCorrelationSignificant)

resumeExtremeEvent <- DocPlanHighestGrowthDecay(dataset[["DateTime"]], dfExtremeEvent)
resumeResult <- paste(resumeIntro, resumeTrend, resumeComparsion, resumeRepeated, resumeExtremeEvent, resumeMotifDiscovery, resumeCorrelation)




# 
currentIntro <- ReadCurrentIntro(dataset[nrow(dataset),"DateTime"])
currentDesc <- CurrentDesc(interpreterNow, vectorTrendDescriptionAnalysis, datasetNumericalWithoutDate)
currentHighest <- CurrentHighest(datasetNow, statisticalResume, datasetIntervalValue)
currentResult <- paste(currentIntro, currentDesc, currentHighest)


# PREDICT LEXICAL PROCESS
i <- 1
vectorTrendDescriptionPredict <- c()
for(i in i:length(datasetNumericalWithoutDate)){
  lastIndex <- interpreterLast$InterpreterIndex[i]
  nowIndex <- interpreterNow$InterpreterIndex[i]
  predictIndex <- interpreterPredict$InterpreterIndex[i]
  vectorSequenceIndex <- c(lastIndex, nowIndex, predictIndex)
  # print(vectorSequenceIndex)
  vectorTrendDescriptionPredict[i] <- LD_Compare(vectorSequenceIndex)
}


# STRUCTURE REALISATION
predictIntro <- ReadPredictIntro(ReadIntro(type="Predict"))

specialCorpus <- IsSpecialCorpusAvailable(interpreterPredict, columnNameNumerical)
if(!is.null(specialCorpus$Sentence)){
  predictIntro <- paste(predictIntro, specialCorpus$Sentence)
}

# STRUCTURE REALISATION
predictContent <- PredictDesc(interpreterPredict,vectorTrendDescriptionPredict,datasetWithoutDate)
predictResult <- paste(predictIntro, predictContent)


# STRUCTURE REALISATION
resumeResult <- PostProcessing(resumeResult)
currentResult <- PostProcessing(currentResult)
predictResult <- PostProcessing(predictResult)

print(resumeResult)
print(currentResult)
print(predictResult)

source("plot.R", local = TRUE)
print("---FINISH---")