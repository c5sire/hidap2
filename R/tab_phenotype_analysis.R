tab_phenotype_analysis <- function(){
  tabItem(tabName = "fieldbook_analysis",
     fluidRow(
            tabBox(width = 12, #collapsible = TRUE,
                title = "Field trial",
                tabPanel("Fieldbook",{
                  textOutput("fb_fieldbook_title")
                  rHandsontableOutput("hotFieldbook", height = 400)
                }),
                tabPanel("Minimal",{
                  rHandsontableOutput("hotMinimal", height = 400)
                })
            )


            # tabBox(width = 12, collapsible = TRUE,
            #     title = "Installation",
            #     tabPanel(
            #       rHandsontableOutput("hotInstallation", height = 400)
            #     )
            #
            # )
          ),
          fluidRow(
            tabBox(width = 12,


                   tabPanel(title = "Fieldmap",
                            textOutput("fb_fieldmap_title"),
                            d3heatmap::d3heatmapOutput("fb_fieldmap_check")
                   ),
                  tabPanel(title = "Correlations (interactive)",
                           qtlcharts::iplotCorr_output('vcor_output')
                  ),
                  tabPanel(title = "Reports",
                           htmlOutput("fb_report")

                  )

            )
          )
  )
}
