tab_phenotype_analysis <- function(){
  tabItem(tabName = "fieldbook_analysis",
          # fluidRow(
          #   box(width = 4, title = "Data source", collapsible = TRUE,
          #       solidHeader = TRUE, status = "warning",
          #       selectInput("fb_analysis_crop", "Select a crop:",
          #       choices = fbcrops::get_crop_table()),
          #       uiOutput("fieldbook_list")
          #   ),
          #   box(width = 4, title = "Definition of factors",collapsible = TRUE,
          #       solidHeader = TRUE, status = "warning",
          #       selectInput("fb_def_plot", "Select plot:",
          #                   choices = "PLOT"),
          #       selectInput("fb_def_reps", "Select repetition:",
          #                   choices = "REP"),
          #       selectInput("fb_def_geno", "Select genotype factor:",
          #                   choices = "INSTN")
          #   ),
          #   box(width = 4, title = "Analysis",collapsible = TRUE,
          #       solidHeader = TRUE, status = "warning",
          #       selectInput("fb_analysis", "Select analysis:",
          #                   choices = c("Descriptive"="descriptive","ANOVA"="aov"))
          #
          #   )
          # ),
          fluidRow(
            box(width = 12,
                title = "Table",
                rHandsontableOutput("hotFieldbook", height = 300)
            )
          ),
          fluidRow(
            tabBox(width = 12,
                  # tabPanel(title = "Fieldmap",
                  #           d3heatmap::d3heatmapOutput("fb_correlations")
                  # ),
                  tabPanel(title = "Fieldmap",
                        textOutput("fb_fieldmap_title"),
                        d3heatmap::d3heatmapOutput("fb_fieldmap")
                  ),
                  tabPanel(title = "Report",
                           htmlOutput("fb_report")
                  )
            )
          )
  )
}
