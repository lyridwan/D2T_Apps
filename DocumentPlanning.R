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
        
        result <- paste(result, "trend is decreased and the rest is almost constant.")
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
      result <- "trend of all variable is decreased."
    }else if(is.na(freq["0"]) && is.na(freq["-"])){
      # ALL +
      result <- "trend of all variable is increased."
      
    }else if(is.na(freq["+"]) && is.na(freq["-"])){
      # ALL 0
      result <- "trend of all variable is constant."
      
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
          phrase <- change_word_bank_AQ(vectorTrendDesc[i])
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
        
        #after grouping the column
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

DocPlanHighestGrowthDecay <- function (dateTime, dfGrowth){
  if(length(dfGrowth) != 0){
    flResult <- c()
    incResult <- c()
    decResult <- c()
    #JPY increased greatly (4.3530 point) from 1st Aug to 1st Oct 2008
    
    i <- flIndex <- incIndex <- decIndex <-1
    for(i in i:nrow(dfGrowth)){
      # index <- rownames(dfGrowth)[i]
      
      
      sentence <- ""
      if(dfGrowth$IncInterpreter[i] == "extreme" && dfGrowth$DecInterpreter[i] == "extreme"){
        event <- "fluctuated"
        adverb <- AdjectiveRefferingExpression(type="ExtremeEvent")
        phrase <- paste(event, adverb)
        
        # sentence <- paste0(dfGrowth$columnNameNumerical[i], " ", 
        #                    phrase, " (increased ", dfGrowth$IncValue[i]," points",
        #                    " from ", incdateRange,
        #                    " and decreased ", abs(dfGrowth$DecValue[i])," points",
        #                    " from ", decdateRange,")")
        
        sentence <- paste0(dfGrowth$columnNameNumerical[i], " ", 
                           phrase, " (increased ", dfGrowth$IncValue[i]," points ",
                           "and decreased ", abs(dfGrowth$DecValue[i])," points)")
        
        flResult[flIndex] <- as.character(sentence)
        flIndex <- flIndex  +1
      }else if(dfGrowth$IncInterpreter[i] == "extreme"){
        event <- "increased"
        adverb <- AdjectiveRefferingExpression(type="ExtremeEvent")
        phrase <- paste(event, adverb)
        
        dateRange <- LexicalDateRange(as.character(dateTime[dfGrowth$IncStartIndex[i]]), 
                                      as.character(dateTime[dfGrowth$IncEndIndex[i]]))
        
        sentence <- paste0(dfGrowth$columnNameNumerical[i], " ", 
                           phrase, " from ", dateRange, " ",
                           "(increased ", dfGrowth$IncValue[i]," points)" ) 
        
        incResult[incIndex] <- as.character(sentence)
        incIndex <- incIndex  +1
      }else if(dfGrowth$DecInterpreter[i] == "extreme"){
        event <- "decreased"
        adverb <- AdjectiveRefferingExpression(type="ExtremeEvent")
        phrase <- paste(event, adverb)
        
        dateRange <- LexicalDateRange(as.character(dateTime[dfGrowth$DecStartIndex[i]]), 
                                      as.character(dateTime[dfGrowth$DecEndIndex[i]]))
        
        sentence <- paste0(dfGrowth$columnNameNumerical[i], " ", 
                           phrase, " from ", dateRange, " ",
                           "(decreased ", abs(dfGrowth$DecValue[i])," points)" )
        
        decResult[decIndex] <- as.character(sentence)
        decIndex <- decIndex  +1
      }
      
    }
    
    result <- AggResumeGrowth(incResult, decResult, flResult)
  }else{
    result <- ""
  }
  
  return(result)
}

MotifDiscoveryDocPlan <- function(listMD){
  listCategorical <- mainConfig[which(mainConfig$Type == ("character") | mainConfig$Type == ("categorical")| mainConfig$Type == ("factor")),]
  
  
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

IsSpecialCorpusAvailable <- function(interpreterPredict, column){
  case1 <- c("Rainfall","CloudCoverage")
  case2 <- c("Temperature")
  case3 <- c("CO","O3","PM10","PM25","SO2")
  
  boolcase1 <- case1 %in% mainConfig[!is.na(mainConfig$Rule),]$ColName
  boolcase2 <- case2 %in% mainConfig[!is.na(mainConfig$Rule),]$ColName
  boolcase3 <- case3 %in% mainConfig$ColName
  
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
    #get all value when list$column value == temperature
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
    
    AQdataLast <- dataset[nrow(dataset)-1,case3]
    AQdatanow <- dataset[nrow(dataset),case3]
    AQdataPredict <- PredictDataset(dataset)
    
    AQvalLast <- AirQualityCalculation(AQdataLast)
    AQvalnow <- AirQualityCalculation(AQdatanow)
    AQvalPredict <- AirQualityCalculation(AQdataPredict)
    #need to be updated
    AQsequence <- LD_Compare(c(AQvalLast, AQvalnow, AQvalPredict))
    
    AQintro <- "air quality will"
    AQtrendDesc <- change_word_bank_AQ(AQsequence)
    AQstate<- AQDataInterpreterAdjective(AQvalPredict)$InterpreterResult
    
    AQsentence <- paste(AQintro, AQtrendDesc, AQstate, ".")
    
    result <- paste0(result, AQsentence)
    vectorResult <- c(vectorResult, case3)
  }
  
  print(mainConfig)
  # print(vectorResult)
  return(list(Sentence = result, VectorResult = vectorResult))
}

RepeatedEventDocPlanning <- function(listRepeated){
  #CONTENT DETERMINATION
  i <- 1
  maxValue <- 0
  maxIndex <- 0
  for(i in i:length(listRepeated)){
    if(listRepeated[[i]]$RepValue > maxValue){
      maxValue <- listRepeated[[i]]$RepValue
      maxIndex <- i
    }
  }
  
  if(maxValue != 0){
    i <- 1
    vectorRepeatedInterpretResult <- c()
    selectedColumn <- columnNameNumerical[maxIndex]
    for(i in i:length(listRepeated[[maxIndex]]$Start)){
      selectedIndex <-listRepeated[[maxIndex]]$Start[i]
      selectedValue <- datasetWithoutDate[[selectedColumn]][selectedIndex]
      
      #DATA INTERPRETATION
      vectorRepeatedInterpretResult[[i]] <- DataInterpreterAdjective(selectedValue, selectedColumn, statisticalResume)$InterpreterResult
    }
  }
  
  #DOCUMENT STRUCTURING
  resumeRepeatedLimit <- as.integer(nrow(dataset) * 0.1)
  resumeRepeatedInterval <- paste0(DataInterpreterInterval(datasetIntervalValue, type = "default"), "s")
  if(maxValue != 0){
    resumeRepeated <- ResumeRepeated2(columnNameNumerical[[maxIndex]], datasetNumerical, vectorRepeatedInterpretResult, listRepeated[[maxIndex]]$Start, listRepeated[[maxIndex]]$End)
    resumeRepeated <- paste("There were some repeating value more than @limit @interval: ", resumeRepeated)
  }else{
    resumeRepeated <- "There were no repeating values within @limit @interval or more, every value changed from time to time."
  }
  
  #Referring Expression Generation 
  resumeRepeated <- gsub("@limit", resumeRepeatedLimit, resumeRepeated)
  resumeRepeated <- gsub("@interval", resumeRepeatedInterval, resumeRepeated)
  
  return(resumeRepeated)
}

correlationRoutineDocPlan <- function(value, parameter){
  #Document Structuring
  # (X) appears to have a highest direct impact to all variable with very strong relationship in average.
  
  interpreterResult <- CorrelationInterpreterAdjective(value)
  result <- ""
  result <- paste0(parameter, " appears to have a highest impact to all variable with ",
                   interpreterResult$InterpreterResult, " in average.")
  return(result)
}

CorrelationSignificantMsgContentDetermination <- function(matrix){
  # ContentDetermination
  # Only showing var more than 0.7
  
  matrix[!lower.tri(matrix)] <- 0
  matrix <- as.data.frame(matrix)
  matrix[matrix < 0.7 & matrix >= 0 | matrix > -0.7 & matrix <= 0] <- 0
  
  # Example: 
  
  # PM2.5       DEWP       TEMP PRES LWS IS IR
  # PM2.5     0  0.0000000  0.0000000    0   0  0  0
  # DEWP      0  0.0000000  0.0000000    0   0  0  0
  # TEMP      0  0.8580855  0.0000000    0   0  0  0
  # PRES      0 -0.7387440 -0.7730187    0   0  0  0
  # LWS       0  0.0000000  0.0000000    0   0  0  0
  # IS        0  0.0000000  0.0000000    0   0  0  0
  # IR        0  0.0000000  0.0000000    0   0  0  0
  
  return(matrix)
}