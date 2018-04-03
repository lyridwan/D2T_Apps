library(shiny)

# Define UI for application that draws a histogram
shinyUI(
fluidPage( 

  source("D2T_machine.R",local=TRUE),
  tags$head(
    tags$meta(charset="utf-8"),
    tags$meta('http-equiv'="X-UA-Compatible", content="IE=edge"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$meta(name="description", content=""),
    tags$meta(name="author", content=""),

    #bootstrap core css
    tags$link(href="bootstrap/vendor/bootstrasdap/css/bootstrap.min.css", rel="stylesheet"),

    #CSS theme
    tags$link(href="bootstrap/css/freelancer.css", rel="stylesheet"),

    #custom font
    tags$link(href="bootstrap/vendor/font-awesome/css/font-awesome.min.css", rel="stylesheet", type="text/css"),
    tags$link(href="https://fonts.googleapis.com/css?family=Montserrat:400,700", rel="stylesheet", type="text/css"),
    tags$link(href="https://fonts.googleapis.com/css?family=Lato:400,700,400italic,700italic", rel="stylesheet", type="text/css")

  ),

    tags$body(id="page-top",class="index",
        tags$div(id="skipnav",
        tags$a(href="#maincontent","Skip to main content")
        ),
        tags$nav(
          tags$div(id="mainNav", class="navbar navbar-default navbar-fixed-top navbar-custom",
            tags$div(class="container",
                # <!-- Brand and toggle get grouped for better mobile display -->
                tags$div(class="navbar-header page-scroll",
                    tags$button(type="button", class="navbar-toggle", 'data-toggle'="collapse", 'data-target'="#bs-example-navbar-collapse-1",
                        tags$span(class="sr-only","Toggle navigation"), HTML("Menu"),
                        tags$i(tags$div(class="fa fa-bars"))
                    ),
                    
                   tags$div(
                          tags$a(href="#page-top",
                          tags$img(src="assets/w-logo.png", width="150px")
                          )
                   )
                    
                ),

              # <!-- Collect the nav links, forms, and other content for toggling -->
                tags$div(class="collapse navbar-collapse", id="bs-example-navbar-collapse-1",
                      tags$ul(class="nav navbar-nav navbar-right",
                          tags$li(class="hidden",
                              tags$a(href="#page-top")
                          ),
                      tags$li(class="page-scroll",
                              tags$a(href="#dataClimates","Weather Data")
                          ),
                      tags$li(class="page-scroll",
                              tags$a(href="#dataAQ","AirQuality Data")
                          ),
                      tags$li(class="page-scroll",
                              tags$a(href="#sitemap","About")
                          )
                      )
                  )
                  # <!-- /.navbar-collapse -->
                )
          )
        ),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$hr(),
          tags$div(class="container",id="maincontent",tabindex="-1",
            tags$div(class="row",
                  tags$div(class="col-lg-12",
                   tags$div(class="col-lg-2",
                    tags$div(class="containerimg",'data-toggle'="modal",'data-target'="#myModal",
                      tags$img(src="assets/light-rain.png", width="100%"),
                      tags$a(href="#",tags$div(class="overlay",tags$div(class="textimg","Show Data")))
                      ),
                    tags$p(align="center", print(InterpretationResult_rainfall),tags$br(),
                    rainfall<-format(round(y[1,"Rainfall"],2),nsmall=2),HTML("mm"),tags$sup("2")
                    ),
                    #print(rainfall),
                      tags$div(class="containerimg", 'data-toggle'="modal",'data-target'="#modalTemperature",

                      tags$img(src="assets/warm.png", width="100%"),
                      
                       tags$a(href="#",tags$div(class="overlay",tags$div(class="textimg","Show Data")))
                      ),
                      tags$p(align="center", print(InterpretationResult_temperature),tags$br(),
                      temperature<-format(round(y[1,"Average.Temperature"],2),nsmall=2),tags$sup("o"),HTML("C")
                      )
                    ),
                    
                   tags$div(class="col-lg-2",
                    tags$div(class="containerimg", 'data-toggle'="modal",'data-target'="#modalAQ",
                      tags$img(src="assets/good.png", width="100%"),
                      tags$a(href="#",tags$div(class="overlay",tags$div(class="textimg","Show Data")))
                      ),
                        tags$p(align="center", print(InterpretationResult_airQuality),tags$br(),
                        HTML("CO :"),AQ<-format(round(AQPredictionResult[1,"CO"],2),nsmall=2),HTML("ppm")
                        ),
                    tags$div(class="containerimg", 'data-toggle'="modal",'data-target'="#modalWind",
                      tags$img(src="assets/breeze.png", width="100%"),
                       tags$a(href="#",tags$div(class="overlay",tags$div(class="textimg","Show Data")))
                      ),
                        tags$p(align="center", print(InterpretationResult_windSpeed),tags$br(),
                        HTML(""),AQ<-format(round(y[1,"Wind.Speed"],2),nsmall=2),HTML("km/h")
                        )
                    ),
                   tags$div(class="col-lg-8",
                      tags$h2("Weather Prediction News"),
                      tags$p(align="justify",HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),
                             Prediction_Result
                          ),
                      tags$hr(class="style13"),
                      tags$p(align="justify",HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),
                            
                             MonthlyMsg
                          )
                    )
                  )
              ),

            tags$section(id="dataClimates",
              tags$div(class="topup",
                tags$hr(),
                tags$p(align="center", tags$h3("Weather Datasets")),
                tags$div(class="row",
                  tags$div(class="col-lg-12",
                    dataTableOutput('dataClimates')
                  )
                )
              )
            ),

            tags$section(id="dataAQ",
              tags$div(class="topup",
                tags$hr(),
                tags$p(align="center", tags$h3("AirQuality Datasets")),
                tags$div(class="row",
                  tags$div(class="col-lg-12",
                    dataTableOutput('dataAQ')
                  )
                )
              )
            ),


   

            #<!-- Modal -->
              tags$div(id="myModal",class="modal fade",role="dialog",
                tags$div(class="modal-dialog",

                  # <!-- Modal content-->
                  tags$div(class="modal-content",
                    tags$div(class="modal-header",
                      tags$button(type="button", class="close", 'data-dismiss'="modal","Data"),
                      tags$h4(class="modal-title")
                    ),
                    tags$div(class="modal-body",
                      plotOutput("plotRainfall", click = "plot_click"),
                      verbatimTextOutput("info"),
                      tags$table(class="table table-hover",
                        tags$tr(
                          tags$td(
                          tags$p("Average in a month")),
                          tags$td(LMmean_result["Rainfall"], "mm2")
                        ),
                        tags$tr(
                          tags$td(
                          tags$p("Todays Value")),
                          tags$td(TodaysWeather$Rainfall, "mm2")
                        ),
                        tags$tr(
                          tags$td(
                          tags$p("Tomorrow Prediction")),
                          tags$td(y[1,"Rainfall"], "mm2")
                        ),
                         tags$tr(
                          tags$td(
                          tags$p("description")),
                          tags$td(InterpretationResult_rainfall)
                        )

                      )
                    ),
                    tags$div(class="modal-footer",
                      tags$button(type="button", class="btn btn-default", 'data-dismiss'="modal","Close")
                    )
                   
                  )
                 )
                ),
              tags$div(id="modalTemperature", class="modal fade",role="dialog",
                tags$div(class="modal-dialog",
                  tags$div(class="modal-content",
                    tags$div(class="modal-header",
                      tags$button(type="button", class="close", 'data-dismiss'="modal","Ranfall Data"),
                      tags$h4(class="modal-title")
                    ),
                    tags$div(class="modal-body",
                      plotOutput("plotTemperature", click = "plot_click"),
                      # verbatimTextOutput("info"),
                      tags$table(class="table table-hover",
                        tags$tr(
                          tags$td(
                          tags$p("Average in a month")),
                          tags$td(LMmean_result["Average.Temperature"], "degree of Celcius")
                        ),
                        tags$tr(
                          tags$td(
                          tags$p("Todays Value")),
                          tags$td(TodaysWeather$Temperature, "degree of Celcius")
                        ),
                        tags$tr(
                          tags$td(
                          tags$p("Tomorrow Prediction")),
                          tags$td(y[1,"Average.Temperature"], "degree of Celcius")
                        ),
                         tags$tr(
                          tags$td(
                          tags$p("description")),
                          tags$td(InterpretationResult_temperature)
                        )
                      )

                    )

                  )
                )
              ),
          tags$div(id="modalWind", class="modal fade",role="dialog",
                tags$div(class="modal-dialog",
                  tags$div(class="modal-content",
                    tags$div(class="modal-header",
                      tags$button(type="button", class="close", 'data-dismiss'="modal","Ranfall Data"),
                      tags$h4(class="modal-title")
                    ),
                    tags$div(class="modal-body",
                      plotOutput("plotWindSpeed", click = "plot_click"),
                      # verbatimTextOutput("info"),
                      tags$table(class="table table-hover",
                        tags$tr(
                          tags$td(
                          tags$p("Average in a month")),
                          tags$td(LMmean_result["Wind.Speed"], "km/h")
                        ),
                        tags$tr(
                          tags$td(
                          tags$p("Todays Value")),
                          tags$td(TodaysWeather$WindSpeed, "km/h")
                        ),
                        tags$tr(
                          tags$td(
                          tags$p("Tomorrow Prediction")),
                          tags$td(y[1,"Wind.Speed"], "km/h")
                        ),
                         tags$tr(
                          tags$td(
                          tags$p("description")),
                          tags$td(InterpretationResult_windSpeed)
                        )
                      )

                    )

                  )
                )
              ),
          tags$div(id="modalAQ", class="modal fade",role="dialog",
                tags$div(class="modal-dialog",
                  tags$div(class="modal-content",
                    tags$div(class="modal-header",
                      tags$button(type="button", class="close", 'data-dismiss'="modal","Ranfall Data"),
                      tags$h4(class="modal-title")
                    ),
                    tags$div(class="modal-body",
                      plotOutput("plotAQ", click = "plot_click"),
                      # verbatimTextOutput("info"),
                      tags$table(class="table table-hover",
                         tags$tr(
                          tags$td(
                          tags$p("description")),
                          tags$td(InterpretationResult_airQuality
                            )
                          ),
                         tags$tr(
                          tags$td(
                          dataTableOutput('AQPredictionResult_data')
                            )
                          )
                      )

                    )

                  )
                )
              )



          )
    ),
    tags$script(src="bootstrap/vendor/jquery/jquery.min.js"),

    # <!-- Bootstrap Core JavaScript -->
    tags$script(src="bootstrap/vendor/bootstrap/js/bootstrap.min.js"),

    # <!-- Plugin JavaScript -->
    tags$script(src="bootstrap/js/jquery.easing.min.js"),

    # <!-- Contact Form JavaScript -->
    tags$script(src="bootstrap/js/jqBootstrapValidation.js"),
    tags$script(src="bootstrap/js/contact_me.js"),

    # <!-- Theme JavaScript -->
    tags$script(src="bootstrap/js/freelancer.min.js"),
    

 #footer
    tags$footer(id="sitemap", class="text-center",
       tags$div(class="footer-above",
            tags$div(class="container",
                tags$div(class="row",
                    tags$div(class="footer-col col-md-4",
                        tags$h3("Data-to-text System"),
                        tags$p(HTML("Monitoring Station of Mabegondo"),tags$br(),HTML("A Coruna City, Spain"))
                    ),
                    tags$div(class="footer-col col-md-4",
                        tags$h3("Researcher"),
                        tags$p(HTML("Brahma Putra"),tags$a(href="www.google.com","brahma.putra96@student.upi.edu")
                          ),
                        tags$ul(class="list-inline",
                            tags$li(
                                tags$a(href="#",class="btn-social btn-outline",
                                  tags$span(class="sr-only","Facebook"),tags$i(class="fa fa-fw fa-google")
                                )
                            ),
                            tags$li(
                                tags$a(href="http://Instagram.com/brahmaptr",class="btn-social btn-outline",
                                  tags$span(class="sr-only","Instagram"),tags$i(class="fa fa-fw fa-instagram")
                                )
                            ),
                            tags$li(
                                tags$a(href="#",class="btn-social btn-outline",
                                  tags$span(class="sr-only","Facebook"),tags$i(class="fa fa-fw fa-linkedin")
                                )
                            )
                        )
                    ),
                    tags$div(class="footer-col col-md-4",
                        tags$h3("Maintainer"),
                        tags$p(HTML("Lala Septem Riza, Ph.D"),tags$a(href="www.google.com","lala_s_riza@yahoo.com")
                          ),
                        tags$p(HTML("Yaya Wihardi, M.Kom"),tags$br(),tags$a(href="www.google.com","yaya@upi.edu")
                          )
                    )
                
            )
        ),
        tags$div(class="footer-below",
            tags$div(class="container",
                tags$div(class="row",
                    tags$div(class="col-lg-12",
                        "DEPARTEMEN ILMU KOMPUTER UPI 2017"
                    )
                )
            )
        )
        )
    )


#fluid page
    )
#shiny UI
  )

