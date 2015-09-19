tab_environment_dashboard <- function(){
  tabItem(tabName = "dashboard_environment",
          fluidRow(
            box(width = 12,
                leafletOutput("map")
                ),
            box(width = 4, title="Site", htmlOutput("site_desc")),
#             box(width = 4, title="Yield across sites",
#                 plotOutput("hist_nvar", height = 250)),
            box(width = 4, #title="Top ten varieties",
                plotOutput("dot_yield", height = 250)),
            box(width = 4, title = "Location summary report",
                htmlOutput("rep_loc"))
          )
  )
}
