# setwd("~/GitHub/D2T_Apps")

# INITIALIZING
source("D2T_Machine.R", local = TRUE)


# READ DATA
dataset <- read.table(file="Datasets/dummy.csv", sep=",", header=TRUE)
datasetWithoutDate <- dataset[ , colnames(dataset) != "DateTime"]
# airQualityDataset <- read.table(file="Datasets/AQ_2016_2017.csv", sep=",", header=TRUE)

#  
columnName <- colnames(dataset[ , colnames(dataset) != "DateTime"])

# Predict
datasetPredicted <- PredictDataset(dataset)


# SYGNAL ANALYSIS

# row [N] data, now
datasetNow <- dataset[nrow(dataset), !colnames(dataset) == "DateTime"]

# row[N-1] data, before now
datasetLast <- dataset[nrow(dataset)-1, !colnames(dataset) == "DateTime"]

# Average Resume
averageResume <- as.data.frame.list(colMeans(dataset[, !colnames(dataset) == "DateTime"]))

# Statistical Resume
statisticalResume <- StatisticalAnalysis(dataset)

i <- 1
vectorTrendAnalysisResult <- c()
for(i in i:length(datasetWithoutDate)){
  vectorColumn <- datasetWithoutDate[[i]]
  vectorTrendAnalysisResult[i] <- TrendAnalysis(1, vectorColumn)
}

#merging DF with trend column
statisticalResume$Trend <- vectorTrendAnalysisResult

# DATA INTERPRETATION
interpreterNow <- DataInterpreter(datasetNow,statisticalResume)
interpreterLast <- DataInterpreter(datasetLast,statisticalResume)
interpreterResume <- DataInterpreter(averageResume,statisticalResume)

# i=1;
# # Today
# for (i in i:length(columnName)) {
#   cat(" Today", columnName[i], " :", as.character(unlist(interpreterNow[i])), "\n\n")
# }

# i=1;
# # Last
# for (i in i:length(columnName)) {
#   cat(" Last", columnName[i], " :", as.character(unlist(interpreterLast[i])), "\n\n")
# }

# i=1;
# # Resume
# for (i in i:length(columnName)) {
#   cat(" Resume", columnName[i], " :", as.character(unlist(interpreterResume[i])), "\n\n")
# }


resumeIntro <- ReadResumeIntro(dataset["DateTime"], columnName)
trendIntro <- ReadIntro(type="Trend")
resumeTrend <- paste(trendIntro,ResumeTrend(statisticalResume))

eventIntro <- ReadIntro(type="Event")
resumeEvent <- paste(eventIntro,"6 value from Xth to Yth")


currentIntro <- ChangeTimeDesc(ReadIntro(type="Current"),dataset["DateTime"])
currentDesc <- CurrentDesc(interpreterNow, statisticalResume, dataset)
# currentAglast <- TrendAnalysis(length(dataset)-5, dataset[[2]])
# currentAgresume <- "and now is the higest from overall."

predictIntro <- ReadIntro(type="Predict")
predictContent <- "Content content content."
predictConc <- "Conclussion from predict result."




resumeResult <- paste(resumeIntro, resumeTrend, resumeEvent)
# currentResult <- paste(currentIntro, currentDesc, currentAglast, currentAgresume)
currentResult <- paste(currentIntro, currentDesc)

predictResult <- paste(predictIntro, predictContent, predictConc)

source("D2T_Machine.R", local = TRUE)

resumeResult
currentResult
predictResult