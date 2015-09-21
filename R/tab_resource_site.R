tab_resource_site <- function(){
  tabItem(tabName = "resource_site",
          fluidRow(
            #box(plotOutput("plot1", height = 250)),

            box(width = 12,
              title = "Site configuration",
              rHandsontableOutput("hot_sites", height = 600)
            )
          )
  )
}
