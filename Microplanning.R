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
    }else if(interval == 672 || interval == 696 || interval == 720 || interval == 744){
      result <- "This month"
    }else if(interval == 8760 || interval == 8736){
      result <- "This year"
    }else{
      result <- ""
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
             interval == 672 || interval == 696 || interval == 720 || 
             interval == 744 || 
             interval == 8760 || 
             interval == 8736){
      result <- 4
    }else{
      result <- ""
    }
  }else if(type == "lastinterpreter"){
    #hourly data
    if(interval == 1){
      result <- "yesterday"
      #daily data
    }else if(interval == 24){
      result <- "last week"
      #weekly data
    }else if(interval == 168){
      result <- "last  month"
      #monthly data
    }else if(interval == 672 || interval == 696 || interval == 720 || 
             interval == 744 ){
      result <- "last  year"
      #yearly data
    }else if(interval == 8760 || 
             interval == 8736){
      result <- "last  quarter"
    }else{
      result <- ""
    }
  }else if(type == "lastlimit"){
    #hourly data
    if(interval == 1){
      result <- 12
      #daily data
    }else if(interval == 24){
      result <- 7
      #weekly data
    }else if(interval == 168){
      result <- 4
      #monthly data
    }else if(interval == 672 || interval == 696 || interval == 720 || 
             interval == 744 ){
      result <- 12
      #yearly data
    }else if(interval == 8760 || 
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

ReadResumeIntro <- function(dataset, ColName, title, source="data"){
  
  corpus <- as.matrix(read.table(file=paste0("Corpus/","ResumeIntro.csv"), header=FALSE, sep=';'))
  
  #Randoming corpus
  n <- length(corpus)
  random_value <- as.integer(runif(1,1,n))
  result <- corpus[random_value]
  
  #Replaceing Data Source
  result <- gsub("@source", paste(title, source), result)
  
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

ReadPredictIntro <-function (intro) {
  conj <- ReadIntro(type="PredictConj")
  
  intro <- gsub("@conj",conj, intro)
  
  return(intro)
}

LexicalDateRange  <- function(dateStart, dateEnd){
  #FORMAT: mm/dd/yyyy -> "07/01/2018"
  
  startMonth <- as.numeric(substr(dateStart,1,2))
  endMonth <- as.numeric(substr(dateEnd,1,2))
  
  startDate <- as.numeric(substr(dateStart,4,5))
  endDate <- as.numeric(substr(dateEnd,4,5))
  
  startYear <- as.numeric(substr(dateStart,7,10))
  endYear <- as.numeric(substr(dateEnd,7,10))
  
  #Example: 1 June 2018 00:00 to 12:00
  if(datasetIntervalValue == 1){
    result <- paste0(startDate, " ", month.abb[startMonth], " ", startYear, " ", SubstrRight(dateStart,5), 
                     " to ", endDate, " ", month.abb[endMonth], " ", endYear, " ", SubstrRight(dateEnd,5))
    return(result)
  }
  
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

change_word_bank_AQ2 <- function (type="default"){
  if(type == "highest"){
    phrase <- as.matrix(read.table("Corpus/Highest_phrase_bank.csv", header=FALSE, sep=';', quote=""))
  }else{
    phrase <- as.matrix(read.table("Corpus/AQ_phrase_bank2.csv", header=FALSE, sep=';', quote=""))
  }
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

change_word_bank_Cor <- function (fragmentCode){
  phraseAQ <- read.table(file="corpus/Correlation_phrase_bank.csv", sep=",", header=TRUE)
  n=length(phraseAQ); i=1; 
  for(i in i:n){
    m=colnames(phraseAQ[i])
    if(fragmentCode==m){
      j=runif(1,1,n+1)
      return(as.character(phraseAQ[j,i]))
    }
  }
}

AggResumeGrowth <- function(vectorGrowth, vectorDecay, vectorFluctuate){
  i<-1
  
  sentence1 <- ""
  if(length(vectorGrowth) != 0){
    for(i in i:length(vectorGrowth)){
      if(length(vectorGrowth) == 1){
        sentence2 <- paste0(sentence1, vectorGrowth[i], ".")
      }else if(i == length(vectorGrowth) && i != 1){
        sentence1 <- paste0(sentence1, "and ", vectorGrowth[i], ".")
      }else{
        sentence1 <- paste0(sentence1, vectorGrowth[i], ", ")
      }
    }
  }
  
  sentence2 <- ""
  if(length(vectorDecay) != 0){
    i<-1
    for(i in i:length(vectorDecay)){
      if(length(vectorDecay) == 1){
        sentence2 <- paste0(sentence2, vectorDecay[i], ".")
      }else if(i == length(vectorDecay) && i !=1){
        sentence2 <- paste0(sentence2, "and ", vectorDecay[i], ".")
      }else{
        sentence2 <- paste0(sentence2, vectorDecay[i], ", ")
      }
    }
  }
  
  sentence3 <- ""
  if(length(vectorFluctuate) != 0){
    i<-1
    for(i in i:length(vectorFluctuate)){
      if(length(vectorFluctuate) == 1){
        sentence2 <- paste0(sentence3, vectorFluctuate[i], ".")
      }else if(i == length(vectorFluctuate) && i !=1){
        sentence3 <- paste0(sentence3, "and ", vectorFluctuate[i], ".")
      }else{
        sentence3 <- paste0(sentence3, vectorFluctuate[i], ", ")
      }
    }
  }
  
  result <- paste(sentence1, sentence2, sentence3)
  return(result)
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
    if(!is.null(listColumn)){
      verb <- AdjectiveRefferingExpression(type="MotifDiscovery")
      MDintro <- paste0("For the past ", limit, " ", interval, " ,")
      MDcontent <- paste("no", verb, "patterns were found for each categorical parameters.")
      
      MDsentence <- paste(MDintro, MDcontent)
      return(MDsentence)
    }else{
      return("")
    }
    #Aggregation
  }else{
    #There are a matching USD data pattern in the last 7 days 
    #with data patterns from August 22-24 2018 and September 23-25 2018.
    
    #if pattern found only 1 parameter
    if(sum(!is.na(listColumn)) == 1){
      selectedColindex <- as.numeric(listColumn[which(!is.na(listColumn))])
      selectedColname <- columnName[selectedColindex]
      verb <- AdjectiveRefferingExpression(type="MotifDiscovery")
      
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
AdjectiveRefferingExpression <- function (type){
  phrase <- as.matrix(read.table(paste0("Corpus/",type,"RE.csv"), header=FALSE, sep=';', quote=""))
  # print(corpus)
  n <- length(phrase)
  random_value <- as.integer(runif(1,1,n+0.5))
  
  result <- phrase[random_value]
  return (result)
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
  
  if(nrow(result) != 0){
    i<-1
    for(i in i:nrow(result)){
      corpus <- gsub(as.character(result$ColName[i]), as.character(result$Alternate[i]), corpus)
    }
  }
  
  
  corpus <- gsub("   ", " ", corpus)
  corpus <- gsub("  ", " ", corpus)
  return(corpus)
}

CorrelationSignificantMsgAggregation <- function(x, parameter, category){
  
  # x 
  # [1] "TEMP"
  # parameter 
  # [1] "DEWP"
  # category
  # [1] "posInc"
  
  # REMOVING DUPLICATE
  type <- unique(category)
  # type
  # [1] "posInc"
  
  df <- data.frame(parameter, category)
  # > df
  #   parameter category
  # 1      DEWP   posInc
  
  result <- ""
  i<-1
  # LOOPING WITH UNIQUE CATEGORY
  for(i in i:length(type)){
    # SUM, Checking if category has multiple value
    totalType <- sum(category == type[i])
    
    groupedPar <- ""
    
    # If selected cat only have 1 value
    if(totalType == 1){
      groupedPar <- paste0(as.character(df[df$category == type[i],]$parameter))
      # groupedPar
      # [1] "DEWP"
      
      # If selected category has multiple value
    }else{
      j<-1
      # loop as multiple value
      for(j in j:nrow(df[df$category == type[i],])){
        # if last data, add "and" phrase
        if(j == nrow(df[df$category == type[i],])){
          groupedPar <- paste0(groupedPar, "and ")
          groupedPar <- paste0(groupedPar, as.character(df[df$category == type[i],]$parameter[j]))
        }else{
          groupedPar <- paste0(groupedPar, as.character(df[df$category == type[i],]$parameter[j]), ", ")
        }
      }
    }
    
    #REPLACING Param X with groupedPar
    # grouped par example: DEWP, TEMP, and PM2.5
    sentence <- change_word_bank_Cor(type[i])
    sentence <- gsub("@X", x, sentence)
    sentence <- gsub("@Y", groupedPar, sentence)
    
    result <- paste(result, sentence)
  }
  
  # print("----")
  # print(result)
  return(result)
}

CurrentHighestAggregation <- function(dataName, result, index, interval){
  thisInterval <- DataInterpreterInterval(interval)
  
  sentence1 <- ""
  sentence2 <- ""
  message <- ""
  
  if(sum(!is.na(result) == TRUE) != 0){
    result <- result[!is.na(result)]
    index <- index[!is.na(index)]
    
    reps<- rep(0, length(result))
    
    if(sum(result == "-") == 1){
      sentence1 <- paste0(names(dataName)[index[[which(result == "-")]]], " reached their lowest value on this ", thisInterval, ".")
    }else if(sum(result == "-") > 1){
      i<-1
      groupedMsg <- ""
      for(i in i:length(result)){
        # print(i)
        if(result[i] == "-"){
          if(i == length(result)){
            groupedMsg <- paste0(groupedMsg, " and ", names(dataName)[index[i]])
          }else{
            
            groupedMsg <- paste0(groupedMsg, names(dataName)[index[i]], ",")
          }
        }
      }
      
      sentence1 <- paste0(groupedMsg, " reached their lowest value on this ", thisInterval, ".")
    }
    
    
    if(sum(result == "+") == 1){
      sentence2 <- paste0(names(dataName)[index[[which(result == "+")]]], " reached their highest value on this ", thisInterval, ".")
    }else if(sum(result == "+") > 1){
      i<-1
      groupedMsg <- ""
      for(i in i:length(result)){
        # print(i)
        if(result[i] == "+"){
          if(i == length(result)){
            groupedMsg <- paste0(groupedMsg, "and ", names(dataName)[index[i]])
          }else{
            
            groupedMsg <- paste0(groupedMsg, names(dataName)[index[i]], ", ")
          }
        }
      }
      
      sentence2 <- paste0(groupedMsg, " reached their highest value on this ", thisInterval, ".")
    }
    
  }
  message <- paste(sentence1, sentence2)
  
  return(message)
}

ComparsionAggregation <- function(group){
  if(is.null(group)){
    return(group)
  }else if(length(group) == 1){
    return(group)
  }else{
    i <- 1
    message <- ""
    for(i in i:length(group)){
      if(i == length(group)){
        message <- paste0(message, "and ", group[i])
      }else{
        message <- paste0(message, group[i], ", ") 
      }
    }
    
    return(message)
  }
}
  