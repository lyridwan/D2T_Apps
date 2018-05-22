
# INITIALIZING
source("D2T_Machine.R", local = TRUE)


# READ DATA
dataset <- read.table(file="Datasets/dummy.csv", sep=",", header=TRUE)
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


source("dummy2.R", local = TRUE)
resumeIntro <- ReadResumeIntro(dataset["DateTime"], columnName)
trendIntro <- ReadIntro(type="Trend")
resumeTrend <- paste(trendIntro,"TREND TREND aaaaaaa")

eventIntro <- ReadIntro(type="Event")
resumeEvent <- paste(eventIntro,"6 value from Xth to Yth")


currentIntro <- ChangeTimeDesc(ReadIntro(type="Current"),dataset["DateTime"])
currentDesc <- CurrentDesc(interpreterLast)
currentAglast <- TrendAnalysis(length(dataset)-5, dataset)
currentAgresume <- "and now is the higest from overall."

predictIntro <- ReadIntro(type="Predict")
predictContent <- "Content content content."
predictConc <- "Conclussion from predict result."

resumeResult <- paste(resumeIntro, resumeTrend, resumeEvent)

currentResult <- paste(currentIntro, currentDesc, currentAglast, currentAgresume)

predictResult <- paste(predictIntro, predictContent, predictConc)

resumeResult
currentResult
predictResult