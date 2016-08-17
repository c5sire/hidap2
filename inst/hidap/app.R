library(brapi)
library(brapps)
library(shinyFiles)
library(DT)
library(agricolae)
library(dplyr)
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


#
#
# brapi_host = "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu"
# #globalVariables(c("values", "crop", "mode"))
#
# get_plain_host <- function(){
#   host = stringr::str_split(Sys.getenv("BRAPI_DB") , ":80")[[1]][1]
#   if(host == "") host = brapi_host
#   if(stringr::str_detect(host, "@")){
#     if(stringr::str_detect(host, "http://")) {
#       host = stringr::str_replace(host, "http://", "")
#     }
#     host = stringr::str_replace(host, "[^.]{3,8}:[^.]{4,8}@", "")
#   }
#   host
# }
#
# host = get_plain_host()

ui <- dashboardPage(skin = "yellow",

                    dashboardHeader(title = "HIDAP4RTB",
                                    dropdownMenuOutput("messageMenu")

                    ),

                    dashboardSidebar(
                      #div(style="margin-right: auto;",img(src = "Logo1.png", width = "230")),
                      sidebarMenu(id = "menu",
                                  # menuItem("Summary",
                                  #  menuSubItem("Summary Dashboard", tabName = "dashboard_summary", icon = icon("dashboard"),
                                  #     selected = TRUE)
                                  # ),
                                  #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), badgeLabel = "new", badgeColor = "green"),
                                  # menuItem("Data tools", icon= icon("folder-open"),
                                  #          #menuSubItem("Data sources", tabName = "dashboard_source"),
                                  #          #menuSubItem("Data import", tabName = "dashboard_import"),
                                  #           menuSubItem("Data checks", tabName = "dashboard_check")
                                  # ) ,
                                  menuItem("Material management", icon = icon("list"),
                                           menuSubItem("Manage lists", icon = icon("list-ol"),
                                                       tabName = "mlt_new"),

                                           menuSubItem("Clone list", icon = icon("paste"),
                                                       tabName = "mlt_clone"),

                                           menuSubItem("Family list", icon = icon("list"),
                                                       tabName = "mlt_family")


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

                                           menuSubItem("Multi-Environment trial analysis",
                                                       tabName = "phe_met", icon = icon("calculator")),

                                           menuSubItem("ELston index",
                                                       tabName = "phe_elston", icon = icon("calculator")),

                                           menuSubItem("Pesek Baker index",
                                                       tabName = "phe_dashboard", icon = icon("calculator"))

                                           #numericInput("fbaInput", "Fieldbook ID", 142, 1, 9999)


                                  ),

                                  menuItem("Environment", tabName = "env_dashboard", icon = icon("globe")
                                  )
                                  ,
                                  menuItem("About", tabName = "about_dashboard", icon = icon("dashboard"),
                                           selected = TRUE,
                                           badgeLabel = "new", badgeColor = "green"),


                                  HTML("<div style='display:none'>"),
                                  shinyURL.ui(label = "",width=0, copyURL = F, tinyURL = F),
                                  #shinyURL.ui("URL", tinyURL = F)
                                  HTML("</div>")




)

),
                    dashboardBody(
                      #tags$head(tags$style(HTML(mycss))),
                      tabItems(
                        tabItem(tabName = "about_dashboard",
                                h2("Highly Interactive Data Analysis Platform for Clonal Plant Breeding"),


                                br(),

                                img(src="potato2.png", width = "100%"),

                                br(),
                                br(),

                                "HIDAP v1.0 [07/06/2016]",
                                p(class = "text-muted", style="text-align:justify",
                                  paste("HIDAP is a tool designed to help breeders of clonal plants (likw potato and sweetpotato) carry out field trial planning, documentation, analysis and reporting")
                                ),
                                #div(style="margin-right: auto;",img(src = "Logo1.png", width = "230"))
                                tags$div(style = "color: #9b9691;float: right;", "International Potato Center (CIP)",
                                         img(src = "Logo1.png", width = "230"),
                                         img(src="gt4sp.png", width = 100)),

                                #br(),
                                #tags$div(style = "horizontal-align: middle;", img(src="gt4sp.png", width = 100)),
                                br(),
                                br()
                        ),
                        tabItem(tabName = "env_dashboard",
                                fluidRow(
                                  column(width = 8
                                         ,
                                         tabBox(width = NULL, id = "tabLocation",
                                                tabPanel("Map",
                                                         leafletOutput("mapLocs")
                                                )
                                                ,
                                                tabPanel("Report",
                                                         htmlOutput("rep_loc")
                                                         #HTML("<h1>Under development!</h1>")
                                                )
                                         )
                                  )
                                  ,
                                  column(width = 4,
                                         tabBox(width = NULL, title = "Site"
                                                ,
                                                tabPanel("Histogram",
                                                         plotOutput("histogram")
                                                )
                                                ,
                                                tabPanel("Info",
                                                         htmlOutput("siteInfo")
                                                )
                                                ,
                                                tabPanel("Fieldtrials",
                                                         htmlOutput("site_fieldtrials")
                                                )
                                                # TODOD
                                                ,
                                                tabPanel("Genotypes",
                                                         htmlOutput("site_genotypes")
                                                )

                                         )
                                  )
                                ),


                                fluidRow(
                                  column(width = 8
                                         ,
                                         box(width = NULL,
                                             title = "Location table"
                                             ,
                                             #p(class = 'text-center', downloadButton('locsDL', 'Download Filtered Data')),
                                             DT::dataTableOutput("tableLocs")
                                             #locationsUI("location")
                                         )
                                  )
                                )
                        ),
                        tabItem(tabName = "phe_fb_check",
                                fluidRow((
                                  column(width = 12,
                                         fbcheck::fbcheck_ui(name = "phe_preprocess"))
                                ))),
                        tabItem(tabName = "phe_dashboard",
                                fluidRow(
                                  column(width = 12,
                                         box(width = NULL, collapsible = TRUE,
                                             title = "Fieldbook",
                                             uiOutput("fbList"),
                                             DT::dataTableOutput("hotFieldbook")
                                             #locationsUI("location")
                                         )
                                  )

                                )
                                ,
                                fluidRow(
                                  column(width = 12,
                                         tabBox(width = NULL, #selected = "Map",
                                                id = "tabAnalysis",
                                                tabPanel("Density",
                                                         uiOutput("phDensUI")
                                                         ,
                                                         div(id = "plot-container",
                                                             plotOutput('phDens_output', height = 400)
                                                         )
                                                ),
                                                tabPanel("Correlation",
                                                         uiOutput("fbCorrVarsUI"),
                                                         #tags$img(src = "www/35.gif"),
                                                         #div(id = "plot-container",
                                                             qtlcharts::iplotCorr_output('vcor_output', height = 900)
                                                         #)
                                                ),

                                                tabPanel("Heatmap",
                                                         uiOutput("phHeatCorrVarsUI"),
                                                         d3heatmap::d3heatmapOutput('phHeat_output', height = 1400)
                                                ),
                                                tabPanel("Dendrogram",
                                                         uiOutput("phDendCorrVarsUI"),
                                                         plotOutput('phDend_output', height = 1400)
                                                ),

                                                tabPanel("Map",
                                                         d3heatmap::d3heatmapOutput("fieldbook_heatmap")
                                                )
                                                ,
                                                tabPanel(title = "Report",

                                                         uiOutput("aovVarsUI"),

                                                         radioButtons("aovFormat","Report format",
                                                                      c("HTML", "WORD" #, "PDF"
                                                                        ),
                                                                      inline = TRUE),
                                                         radioButtons("expType", "Experiment type",
                                                                      c("RCBD", "ABD", "CRD", "A01D"), inline = TRUE),
                                                         conditionalPanel("input.expType == 'A01D'",
                                                                      selectInput("block", "BLOCK", c("BLOC", "BLOCK")),
                                                                      numericInput("k", "k", 2, 2, 5, step = 1)
                                                                          ),

                                                         actionButton("fbRepoDo", "Create report!"),
                                                         HTML("<center>"),
                                                         uiOutput("fbRep"),
                                                         HTML("</center>")





                                                )



                                         )
                                  )

                                )
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

  shinyURL.server()

  fbcheck::fbcheck_server(input, output, session, values)

  brapps::fieldbook_analysis(input, output, session, values)

  brapps::locations(input, output, session, values)

  #fbdesign::server_design(input, output, session, values)
  })

})

shinyApp(ui, sv)









