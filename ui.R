library(shiny)

# Define UI for application that draws a histogram
shinyUI(
fluidPage( 

  source("D2T_Main.R",local=TRUE),
  # source("D2T_Machine.R",local=TRUE),
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
                        tags$p(HTML("XY"),tags$a(href="www.google.com","~~@student.upi.edu")
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

