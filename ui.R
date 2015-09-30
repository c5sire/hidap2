library(shinydashboard)
library(rhandsontable)
library(shinyTree)
library(shinyFiles)
library(leaflet)

library(fbsites)
library(fbcrops)

source("R/global.R") # global needs to be loaded first
source("R/utils_program.R")
source("R/utils_program_stage.R")
source("R/utils_material.R")

source("R/tab_phenotype_analysis.R")

source("R/tab_resource_dashboard.R")
source("R/tab_resource_program.R")
source("R/tab_resource_material.R")
source("R/tab_resource_dictionary.R")
source("R/tab_resource_module.R")

source("R/tab_environment_dashboard.R")


dashboardPage(skin = "yellow",

  dashboardHeader(title = "HIDAP4RTB"),

  dashboardSidebar(
    sidebarMenu(id = "menu",
      # menuItem("Summary",
      #  menuSubItem("Summary Dashboard", tabName = "dashboard_summary", icon = icon("dashboard"),
      #     selected = TRUE)
      # ),
      menuItem("Phenotype",
        # menuSubItem("Phenotype Dashboard", tabName = "dashboard_phenotype", icon = icon("dashboard"),
        #          selected = TRUE),
        # menuSubItem("New fieldbook", icon = icon("file"), tabName = "fbDesign"),
        # menuSubItem("Import fieldbook", icon = icon("file-excel-o"), tabName = "fbImport"),
        #
        # menuSubItem("Check fieldbook", icon = icon("check-square"), tabName = "fbCheck"),
        # menuSubItem("Export fieldbook", icon = icon("file-excel-o"), tabName = "fbExport"),
        # menuSubItem("Import images", icon = icon("photo"), tabName = "fbFotoImport"),
        menuSubItem("Analysis", icon = icon("book"), tabName = "fieldbook_analysis",
                    selected = TRUE)
        # ,
        # menuSubItem("Catalogues", icon = icon("book"), tabName = "fbCatalog")
      ),
      # menuItem("Genotype",
      #  menuSubItem("Genotype Dashboard", tabName = "dashboard_genotype", icon = icon("dashboard"))
      # ,
      #  conditionalPanel(
      #    "input.menu == 'dashboard_genotype'",
      #    selectInput("period", "Period", 1:10)
      #  )
      #),
      menuItem("Environment",
       menuSubItem("Environment Dashboard", tabName = "dashboard_environment", icon = icon("dashboard"))
      ),
      # menuItem("Breeding program",
      #  menuSubItem("Program Dashboard", tabName = "dashboard_program", icon = icon("dashboard"))
      # ),
      menuItem("Resources",
       menuSubItem("Resources Dashboard", icon = icon("dashboard"), tabName = "resource_dashboard"
                #selected = TRUE
                ),
       menuSubItem("Crops", icon = icon("leaf"), tabName = "resource_crop"),
       menuSubItem("Breeding programs", icon = icon("crop"), tabName = "resource_program"),
       menuSubItem("Plant materials", icon = icon("star"), tabName = "resource_material"),
        # conditionalPanel(
        #   "input.menu == 'resource_material'",
        #   selectInput("mlist_crop", "Choose a crop:", unique(get_crop_table()$crop_name) ),
        #   selectInput("mlist_year", "Choose a year:", 2000:2050 ),
        #   textInput("mlist_name", "Choose a list name:", "A unique name")
        #
        # )
       # ,
       menuSubItem("Sites", icon = icon("location-arrow"), tabName = "resource_site"),
       menuSubItem("Data dictionary", icon = icon("book"), tabName = "resource_dictionary")
       # ,
       # menuSubItem("Data module", icon = icon("book"), tabName = "resource_module")

       )
    #   ,
    # menuItem( "Sharing",
    #   menuSubItem("Sharing Dashboard", tabName = "dashboard_sharing", icon = icon("dashboard"))
    # )
    # ,
    # menuItem("Help",
    #    menuSubItem("Documentation", tabName = "dashboard_help", icon = icon("dashboard")),
    #    menuSubItem("Tasks", icon = icon("th"), tabName = "widgets2")
    # )
    )

  ),
  dashboardBody(
    tabItems(
      tab_phenotype_analysis(),
      tab_environment_dashboard(),

      tab_resource_dashboard(),
      fbcrops::ui_crop(),
      tab_resource_program(),
      tab_resource_material(),
      fbsites::ui_site(),
      tab_resource_dictionary()#,
      #tab_resource_module()
    )
  )
)
