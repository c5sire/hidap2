library(shinysky)
library(data.table)
library(shinyTree)
library(brapi)
library(brapps)
library(shinyFiles)
library(DT)
library(agricolae)
library(dplyr)
library(openxlsx)
#library(fbmet)
#library(fbhelp)
#library(fbdesign)
library(rhandsontable)
library(shinydashboard)
library(d3heatmap)
library(shinyURL)
library(qtlcharts)
library(leaflet)
library(dplyr)
library(withr)
library(DT)
library(st4gi)
library(tibble)
library(knitr)
library(readxl)
library(countrycode)
library(fbsites)
library(fbmlist)

library(fbcheck)
library(fbmlist)
library(countrycode)
library(shinyjs)
library(DBI)
library(RMySQL)
library(spsurvey)
library(foreign)
library(tools)
library(stringr)
library(shinyBS)
library(fbdesign)
library(fbopenbooks)
library(fbanalysis)
library(traittools)
library(sbformula)
library(pepa)


ui <- dashboardPage(skin = "yellow",

                    dashboardHeader(title = "HIDAP4RTB",
                                    dropdownMenuOutput("messageMenu")

                    ),

                    dashboardSidebar(
                      #div(style="margin-right: auto;",img(src = "Logo1.png", width = "230")),
                      sidebarMenu(id = "menu",
                                  menuItem("Material management", icon = icon("list"),
                                           menuSubItem("Manage lists", icon = icon("list-ol"),
                                                       tabName = "phe_ml_manager"),

                                           menuSubItem("Clone list", icon = icon("paste"),
                                                       tabName = "phe_ml_clone"),

                                           menuSubItem("Family list", icon = icon("list"),
                                                       tabName = "phe_ml_family")


                                           ),

                                  menuItem("Phenotype", icon = icon("leaf"),

                                           menuSubItem("New fieldbook", icon = icon("file"),
                                                       tabName = "phe_fb_new"),

                                           menuSubItem("Open fieldbook", icon = icon("file-o"),
                                                       tabName = "phe_fb_open"),

                                           menuSubItem("Check fieldbook", icon = icon("eraser"),
                                                       tabName = "phe_fb_check"),

                                           menuSubItem("Single trial analysis",
                                                       tabName = "phe_dashboard", icon = icon("calculator")),

                                           menuSubItem("Single trial report",
                                                       tabName = "phe_set_report", icon = icon("calculator")),

                                           menuSubItem("MET analysis",
                                                       tabName = "phe_met", icon = icon("calculator")),

                                           # menuSubItem("MET report",
                                           #             tabName = "phe_set", icon = icon("calculator"))#,

                                           menuSubItem("ELston index",
                                                       tabName = "phe_elston", icon = icon("calculator")),

                                           menuSubItem("Pesek Baker index",
                                                       tabName = "phe_pesek", icon = icon("calculator")),

                                           menuSubItem("Selection response",
                                                       tabName = "phe_rts", icon = icon("calculator"))

                                           #numericInput("fbaInput", "Fieldbook ID", 142, 1, 9999)


                                  ),

                                  menuItem("Environment", tabName = "env_dashboard", icon = icon("globe")
                                  )
                                  ,
                                  menuItem("About", tabName = "about_dashboard", icon = icon("dashboard"),
                                           selected = TRUE,
                                           badgeLabel = "new", badgeColor = "green"),


                                  HTML("<div style='display:none'>"),
                                  #shinyURL.ui(label = "",width=0, copyURL = F, tinyURL = F),
                                  #shinyURL.ui("URL", tinyURL = F)
                                  HTML("</div>")




)

),
                    dashboardBody(
                      #includeCSS(path = "www/bootstrap.min.css"),
                      #tags$head(tags$style(HTML(mycss))),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
                      ),
                      tabItems(
                        tabItem(tabName = "about_dashboard",
                                h2("Highly Interactive Data Analysis Platform for Clonal Plant Breeding"),


                                #br(),

                                img(src="potato2.png", width = "100%"),

                                # br(),
                                #br(),

                                "HIDAP v1.0-preview [07/09/2016]",
                                p(class = "text-muted", style="text-align:justify",
                                  paste("HIDAP is a tool designed to help breeders of clonal plants (likw potato and sweetpotato) carry out field trial planning, documentation, analysis and reporting.")
                                ),
                                #div(style="margin-right: auto;",img(src = "Logo1.png", width = "230"))
                                tags$div(style = "color: #9b9691;float: right;", "International Potato Center (CIP)"),


                                #br(),
                                tags$div(style = "display:inline-block",
                                         img(src = "Logo1.png", width = "230"),
                                         img(src = "gt4sp.png", width = "100")
                                         ),
                                br(),
                                br()
                        ),
                        tabItem(tabName = "env_dashboard",
                                brapps::locations_ui("Trial Location Explorer")
                        ),
                        tabItem(tabName = "phe_fb_new",
                                fluidRow((
                                  column(width = 12,
                                         fbdesign::ui_fieldbook(name = "phe_fb_new"))
                                ))),
                        tabItem(tabName = "phe_fb_open",
                                fluidRow((
                                  column(width = 12,
                                         fbopenbooks::fbopenbooks_ui(name = "phe_fb_new"))
                                ))),
                        tabItem(tabName = "phe_fb_check",
                                fluidRow((
                                  column(width = 12,
                                         fbcheck::fbcheck_ui(name = "phe_preprocess"))
                                ))),
                        tabItem(tabName = "phe_ml_clone",
                                fluidRow((
                                  column(width = 12,
                                         fbmlist::generate_ui("phe_ml_clone"))
                                ))),
                        tabItem(tabName = "phe_ml_manager",
                                fluidRow((
                                  column(width = 12,
                                         fbmlist::managerlist_ui(name = "phe_ml_manager"))
                                ))),
                        tabItem(tabName = "phe_ml_family",
                                fluidRow((
                                  column(width = 12,
                                         fbmlist::createlist_ui(name = "phe_ml_family"))
                                ))),



                        tabItem(tabName = "phe_met",
                                fluidRow((
                                  column(width = 12,
                                         fbmet::met_ui("Multi-Environment Trial Explorer"))
                                ))),

                        tabItem(tabName = "phe_set_report",
                                fluidRow((
                                  column(width = 12,
                                         fbanalysis::single_ui("phe_set"))
                                ))),

                        tabItem(tabName = "phe_elston",
                                fluidRow((
                                  column(width = 12,
                                         fbanalysis::elston_ui("phe_elston"))
                                ))),
                        tabItem(tabName = "phe_pesek",
                                fluidRow((
                                  column(width = 12,
                                         fbanalysis::pbaker_ui("phe_pbaker"))
                                ))),

                        tabItem(tabName = "phe_rts",
                                fluidRow((
                                  column(width = 12,
                                         brapps::rts_ui("phe_rts"))
                                ))),


                        tabItem(tabName = "phe_dashboard",
                                brapps::fbasingle_ui("Single-Environment Trial Explorer")
                        )

                      )        )
)




############################################################

sv <- function(input, output, session) ({

  values <- shiny::reactiveValues(crop = "sweetpotato", amode = "brapi")

  try({
  brapi_con("sweetpotato", "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu",
            80, "rsimon16",
            "sweetpotato")

  #shinyURL.server()

  fbcheck::fbcheck_server(input, output, session, values)

  fbmlist::server_managerlist(input, output, session, values)

  fbmlist::server_generate(input, output, session, values)

  fbmlist::server_createlist(input, output, session, values)


  fbdesign::server_design(input, output, session, values)

  fbdesign::server_design_big(input, output, session, values)

  fbopenbooks::fbopenbooks_server(input, output, session, values)

  fbanalysis::single_server(input, output, session, values)

  fbanalysis::met_server(input, output, session, values)

  fbanalysis::elston_server(input, output, session, values)

  fbanalysis::pbaker_server(input, output, session, values)


  brapps::fieldbook_analysis(input, output, session, values)

  brapps::locations(input, output, session, values)

  #fbdesign::server_design(input, output, session, values)

  fbmet::met_sv(input, output, session, values)

  brapps::rts_sv(input, output, session, values)

  })

})

shinyApp(ui, sv)









