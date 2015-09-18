tab_resource_dashboard <- function(){
  tabItem(tabName = "resource_dashboard",
          fluidRow(
            valueBox(nrow(get_crop_table()), "Crops", icon = icon("leaf")),
            valueBox(nrow(get_program_table()), "Programs", icon = icon("crop")),
            valueBox(50000, "Materials", icon = icon("star"))
          )
  )
}
