tab_resource_program <- function(){
  tabItem(tabName = "resource_program",
          fluidRow(
            box(
              title = "Breeding program configuration",
              rHandsontableOutput("hot_programs")
            ),
            box(
              title = "Breeding program stage configuration",
              rHandsontableOutput("hot_program_stages")
            )
          )
  )
}
