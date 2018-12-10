#Data-to-text System - Ahmad - Ridwan
#----------------------- Packages Requirement -----------------------#
#1. Shiny R
# install.packages("shiny") #-- server & GUI
library(shiny)

#2. Smooth 
# install.packages("smooth") -> #predicting  with ex.smoothing
library(smooth)

#3. mice
#install.packages("mice")
library(mice)

#4. Time-Series data object
# install.packages("xts")
library(xts)

#5. greybox
#install.packages("greybox")
library(greybox)

#6. data.table
#install.packages("data.table")
library(data.table)

#7. corrplot
#install.packages("corrplot")
library(corrplot)


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
  #Initializing
  nullSequence <- rep(NA, length(columnName))
  dfResult <- data.frame(ColName = columnName, Type = nullSequence, Rule = nullSequence, Alternate=nullSequence, stringsAsFactors=FALSE)
  
  # Read config from file
  mainConfig <- read.table("Config/datadescription.csv", header=TRUE, sep=",")
  
  # Load setting from default file
  i<-1
  for(i in i:nrow(mainConfig)){
    tempColumn <- as.character(unlist(mainConfig$ColName[i]))
    dfResult[dfResult$ColName == tempColumn, "ColName"] <- as.character(mainConfig[mainConfig$ColName == tempColumn, "ColName"])
    dfResult[dfResult$ColName == tempColumn, "Type"] <- as.character(mainConfig[mainConfig$ColName == tempColumn, "Type"])
    dfResult[dfResult$ColName == tempColumn, "Rule"] <- as.character(mainConfig[mainConfig$ColName == tempColumn, "Rule"])
    dfResult[dfResult$ColName == tempColumn, "Alternate"] <- as.character(mainConfig[mainConfig$ColName == tempColumn, "Alternate"])
  }
  
  # Checking variable type with typeof()
  headerClass <- ClassHeaderChecker(dataset)
  headerClass <- headerClass[names(headerClass)!="DateTime"]
  
  # Merging R config with Default config
  i<-1
  for(i in i:length(headerClass)){
    tempColumn <- names(headerClass)[i]
    
    if(is.na(dfResult[dfResult$ColName == tempColumn,"Type"])){
      dfResult[dfResult$ColName == tempColumn,"Type"] <- headerClass[names(headerClass) == tempColumn]
    }
  }
  
  return(dfResult)
}

DateInterval <- function(time1, time2){
  time1 <- strptime(time1, "%m/%d/%Y %H:%M")
  time2 <- strptime(time2, "%m/%d/%Y %H:%M")
  
  interval <- difftime(time1, time2)
  interval <- as.numeric(interval, units = "hours")
  
  return(interval)
}

MissingValueHandling <- function(dataset){
  md.pattern(dataset)
  
  #using linear regression
  model <- mice(dataset,m=5, maxit=50,seed=500, method="pmm")
  result <- complete(model)  # generate the completed data.
  
  return(result)
}