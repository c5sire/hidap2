tab_phenotype_analysis <- function(){
  tabItem(tabName = "fieldbook_analysis",
          fluidRow(
            box(width = 4, title = "Data source", collapsible = TRUE,
                solidHeader = TRUE, status = "warning",
                selectInput("fb_analysis_crop", "Select a crop:",
                choices = get_crop_table()),
                uiOutput("fieldbook_list")
            ),
            box(width = 4, title = "Definition of factors",collapsible = TRUE,
                solidHeader = TRUE, status = "warning",
                selectInput("fb_def_plot", "Select plot:",
                            choices = "PLOT"),
                selectInput("fb_def_reps", "Select repetition:",
                            choices = "REP"),
                selectInput("fb_def_geno", "Select genotype factor:",
                            choices = "INSTN")
            ),
            box(width = 4, title = "Analysis",collapsible = TRUE,
                solidHeader = TRUE, status = "warning",
                selectInput("fb_analysis", "Select analysis:",
                            choices = c("Descriptive"="descriptive","ANOVA"="aov"))

            )
          ),
          fluidRow(
            box(width = 6,
                title = "Fieldbook",
                rHandsontableOutput("hotFieldbook", height = 600)
            ),
            box(width = 6, title = "Report",
                solidHeader = TRUE, status = "primary",
                uiOutput("fb_report")

            )
          )
  )
}
