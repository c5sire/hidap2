pdfButton <- function (outputId, label = "PDF", class = NULL)
{
  aTag <- tags$a(id = outputId, class = paste("btn btn-default shiny-download-link",
                                              class), href = "", target = "_blank", icon("pdf"),
                 label)
}


tab_environment_dashboard <- function(){
  tabItem(tabName = "dashboard_environment",
          fluidRow(
            box(width = 8,
                leafletOutput("map")
                ),
            box(width = 4, title="Site", htmlOutput("site_desc")),
#             box(width = 4, title="Yield across sites",
#                 plotOutput("hist_nvar", height = 250)),
            box(width = 8, title = "Location summary report",
                htmlOutput("rep_loc_pdf"),
                htmlOutput("rep_loc")),
            box(width = 4, #title="Top ten varieties",
                plotOutput("dot_yield", height = 250))

          )
  )
}
