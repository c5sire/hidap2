tab_resource_crop <- function(){
  tabItem(tabName = "resource_crop",
          fluidRow(
            #box(plotOutput("plot1", height = 250)),

            box(
              title = "Crop configuration",
              rHandsontableOutput("hot_crops")
            )
          )
  )
}
