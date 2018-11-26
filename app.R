# Define server logic for random distribution app ----
server <- function(input, output) {
  setwd("~/GitHub/D2T_Apps")
  system.time(source("D2T_Main.R", local = TRUE))
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#75AADB", border = "white")
  })
  
  # Generate a summary of the data ----
  output$resumeResult <- renderText({
    resumeResult
  })
  
  # Generate a summary of the data ----
  output$currentResult <- renderText({
    currentResult
  })
  
  # Generate a summary of the data ----
  output$predictResult <- renderText({
    predictResult
  })
  
  # Generate an HTML table view of the head of the data ----
  output$table <- renderTable({
    head(data.frame(x = d()))
  })
  
}

shinyApp(ui = htmlTemplate("www/index.html"), server)