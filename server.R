source("D2T_machine.R",local=TRUE)
library(shiny)


get_ui<-function(){
itog=list()
itog$ui<-codingan
return(itog)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$dataClimates = renderDataTable({
      dataClimates
    },options = list(lengthMenu = c(5,10, 15, 30,50), pageLength = 10))

    output$dataAQ = renderDataTable({
      dataAQ
    },options = list(lengthMenu = c(5, 10, 15,30,50), pageLength = 10))
    
    output$AQPredictionResult_data = renderDataTable({
      t(AQPredictionResult)
    })
    
    output$plotRainfall = renderPlot({
    plot(density(dataClimates$Rainfall))
  	})
  	output$plotTemperature = renderPlot({
    plot(density(dataClimates$Average.Temperature))
  	})
  	output$plotWindSpeed = renderPlot({
    plot(density(dataClimates$Wind.Speed))
  	})
  	output$plotAQ = renderPlot({
    plot(density(dataAQ$CO))
  	})
 	output$info = renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  	})
    
    output$itog = renderUI({
    	get_ui()
    	})

})