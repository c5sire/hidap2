tab_phenotype_analysis <- function(){
  tabItem(tabName = "fieldbook_analysis",
     fluidRow(
            box(width = 12,
                title = "Table",
                textOutput("fb_fieldbook_title"),
                rHandsontableOutput("hotFieldbook", height = 300)
            )
          ),
          fluidRow(
            tabBox(width = 12,
                   tabPanel(title = "Reports",
                            htmlOutput("fb_report")

                   ),

                   tabPanel(title = "Fieldmap",
                            textOutput("fb_fieldmap_title"),
                            d3heatmap::d3heatmapOutput("fb_fieldmap_check")
                   ),
                  tabPanel(title = "Correlations (interactive)",
                           qtlcharts::iplotCorr_output('vcor_output')
                  )

            )
          )
  )
}
