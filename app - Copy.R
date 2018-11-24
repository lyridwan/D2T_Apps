# Define server logic for random distribution app ----
server <- function(input, output) {
  setwd("~/GitHub/D2T_Apps")
  source("D2T_Main.R", local = TRUE)
}

shinyApp(ui = htmlTemplate("www/index.html"), server)

