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
library(fbhelp)
library(fbdesign)
library(rhandsontable)
library(shinydashboard)
library(d3heatmap)
library(shinyURL)
# library(qtlcharts)
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

# init default data: TODO make a function with better logic checking whats new
# from fbglobal get_base_dir
dd = system.file("xdata/Demo", package = "fbglobal")
file.copy(from = dd, to = fbglobal::get_base_dir(""), recursive = TRUE)
dd = system.file("xdata/Default", package = "fbglobal")
file.copy(from = dd, to = fbglobal::get_base_dir(""), recursive = TRUE)



ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "HIDAP", titleWidth = "250px"

  ),#end Header
  dashboardSidebar(width = "250px",

                   #div(style="margin-right: auto;",img(src = "Logo1.png", width = "250")),
                   br(),
                   div(img(src="hidapicon.png", width = "85px"), style="text-align: center;"),

                   #sidebarSearchForm(label = "Enter a word", "searchText", "searchButton"),
                   sidebarMenu(
                     id = "tabs",
                     # menuItem("Phenotype tool", icon = icon("th-list"),
                     menuItem("Phenotype", icon = icon("th-list"),

                              menuItem("Material Management",
                                       menuSubItem("Manage list", tabName = "manageList", icon = icon("table")),
                                       menuSubItem("Clone list", tabName = "generateList", icon = icon("list")),
                                       menuSubItem("Family list", tabName = "createList", icon = icon("list-alt"))#,

                              ),

                              menuItem("Fieldbook management",
                                       menuSubItem("New fieldbook", tabName = "newFieldbook", icon = icon("file")),
                                       menuSubItem("Open fieldbook", tabName = "openFieldbook", icon = icon("file-o")),
                                       menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser"))#,
                              ),

                              menuItem("Single Trial Analysis",
                                       menuSubItem("Single trial graph",tabName = "singleAnalysisGraphs", icon = icon("calculator")),
                                       menuSubItem("Single report", tabName = "singleAnalysisReport", icon = icon("file-text-o"))#,
                              ),

                              menuItem("MET Trial Analysis",
                                       menuSubItem("MET analytical graph",tabName = "metAnalysisGraphs", icon = icon("calculator")),
                                       menuSubItem("MET report", tabName = "metAnalysisReport",icon = icon("file-text-o"))#,
                              ),

                              menuItem("Index Selection",
                                       menuSubItem("Elston index",tabName = "elstonIndex",icon = icon("file-text-o")),
                                       menuSubItem("Pesek-Baker index", tabName = "pesekIndex",icon = icon("indent")),
                                       menuSubItem("Selection response", tabName = "selResponse",icon = icon("indent"))
                              )



                     ),
                     menuItem("About", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE)#,
                     #  ------------------------------------------------------------------------

                   )
                   # )
  ),

  dashboardBody(
    #
    #     tags$head(
    #       tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
    #     ),

    includeCSS("www/custom.css"),

    tabItems(

      ###
      #Codigo Ivan Perez
      tabItem(tabName = "dashboard",
              h2("High Interactive Data Analysis Platform"),

              br(),
              br(),
              #img(src="potato.jpg", width = "100%"),
              img(src="about.png", width = "100%"),

              br(),
              br(),

              "HIDAP Preview [13/09/2016]",
              p(class = "text-muted", style="text-align:justify",
                paste("HIDAP is a tool designed to help plant breeders of clonal crops like potato and sweetpotato to carry out field trial planning, documentation, analysis and reporting")
              ),


              br(),
              br(),

              fluidRow(
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  br(),
                  div(img(src="logo1.png", width = "150px"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  div(img(src="gt4sp.png", height = "108px"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  br(),
                  div(img(src="usaid.png", width = "150px"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  div(img(src="sasha.png"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  br(),
                  div(img(src="rtb.png", width = "150px"), style="text-align: center;")
                )
              ),

              br(),
              br(),
              br()
      ),

      tabItem(tabName = "integration",
              fluidRow(
                box(
                  title = "CIPFBS report", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  tags$iframe(src = "http://176.34.251.32/cipfieldbookstorage_dev/protected/extensions/grid/demo.php",
                              seamless=NA, width = "100%", height = "800px"
                  )
                )
              ),

              br(),
              br(),
              br()
      ),
      #Fin codigo Ivan Perez
      ###

      # Design Experiments Module ----------------------------------------------------
      fbdesign::ui_fieldbook(name = "newFieldbook"),

      # Data Quality and Check Fieldbook Module  ----------------------------------------------------
      fbcheck::fbcheck_ui(name="checkFieldbook"),

      # Fieldbook Manager Module ----------------------------------------------------
      fbopenbooks::fbopenbooks_ui(name="openFieldbook"),

      # Material List Module ----------------------------------------------------

      fbmlist::generate_ui(name = "generateList"),
      fbmlist::managerlist_ui(name = "manageList"),
      fbmlist::createlist_ui(name = "createList"),


      brapps::fbasingle_ui("Single Chart"),
      fbanalysis::single_ui(name="singleAnalysisReport"),
      fbanalysis::met_ui(name="metAnalysisReport"),
      fbanalysis::elston_ui(name="elstonIndex"),
      fbanalysis::pbaker_ui(name="pesekIndex"),

      brapps::rts_ui("selResponse"),


      tabItem(tabName = "analysis",
              h2("Analysis"),
              p(class = "text-muted",
                paste("Under construction...")
              )
      )
    ) , #end of TabSetPanel

    tags$div(
      fluidRow(
        tags$footer(
          a(
            list(
              tags$div(id = "test", img(src="88x31_v2.png"), "2016 International Potato Center. Av La Molina 1895, La Molina - Peru.")
            ),
            href="#"
          ),
          tags$style("footer {background-color: #222d32;height: 40px;position: absolute;bottom: 0;width: 100%;}"),
          tags$style("#test {color: #fff;padding-top: 5px;}")
        )
      )
    )

  )
)




############################################################

sv <- function(input, output, session) ({

  values <- shiny::reactiveValues(crop = "sweetpotato", amode = "brapi")

#
#
#   try({
#   brapi_con("sweetpotato", "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu",
#             80, "rsimon16",
#             "sweetpotato")
#   })

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
  #brapps::locations(input, output, session, values)
  #fbmet::met_sv(input, output, session, values)
  brapps::rts_sv(input, output, session, values)

  # drat::addRepo("c5sire")
  # res = eventReactive(input$about_update, {
  #   cat("Ok")
  #   if(brapi::can_internet()){
  #   withProgress({
  #   try({
  #     update.packages(ask = FALSE)
  #   })
  #   }, message = "Checking for updates ...")
  #   }
  # })
  #
  #
  # })

})

shinyApp(ui, sv)









