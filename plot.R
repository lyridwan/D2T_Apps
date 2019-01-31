
i<- 1
for(i in i:length(datasetNumericalWithoutDate)){
  col <- names(datasetNumericalWithoutDate)[i]
  x <- seq(1:nrow(dataset))
  y <- dataset[[col]]
  
  png(filename=paste0("Plot/",substr(filename, 1, nchar(filename)-2), i, col,".png"), width=500, height=500)
  
  plot(main=col, xlab="Index", ylab="Value", y=y, x=x, type="o", lwd=1)
  
  # regression
  reg = lm(y~x)
  # abline(reg,col="yellow", lwd=2)
  
  # increase
  if(dfExtremeEvent$IncInterpreter[i] == "extreme"){
    start <- dfExtremeEvent$IncStartIndex[i]
    end <- dfExtremeEvent$IncEndIndex[i]
    # lines(x[start:end], y[start:end], col="green", lwd=2)
  }
  
  # decrease
  if(dfExtremeEvent$DecInterpreter[i] == "extreme"){
    start <- dfExtremeEvent$DecStartIndex[i]
    end <- dfExtremeEvent$DecEndIndex[i]
    # lines(x[start:end], y[start:end], col="red", lwd=2)
  }
  
  if(listRepeatedAnalysisResult[[i]]$RepValue != 0){
      j <- 1
      for(j in j:length(listRepeatedAnalysisResult[[i]]$Start)){
        start <- listRepeatedAnalysisResult[[i]]$Start[j]
        end <- listRepeatedAnalysisResult[[i]]$End[j]
        lines(x[start:end], y[start:end], col="blue", lwd=2)
      }
  }
  
  #get highgest/lowest value from statsum
  maxVal <- as.numeric(as.character(statisticalResume[statisticalResume$ColName == col,]$MaxValue))
  minVal <- as.numeric(as.character(statisticalResume[statisticalResume$ColName == col,]$MinValue))
  
  if(datasetNow[i] >= maxVal){
    lines(nrow(dataset), datasetNow[i], col="green", type = "o", lwd=2)
  }else if(datasetNow[i] <= minVal){
    lines(nrow(dataset), datasetNow[i], col="red", type = "o", lwd=2)
  }
  
  dev.off()
}

png(filename=paste0("Plot/",substr(filename, 1, nchar(filename)-2), "Correlation.png"), width=500, height=450)
corrplot.mixed(cor(datasetNumericalWithoutDate), tl.col = "black")
dev.off()
