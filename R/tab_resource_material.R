tab_resource_material <- function(){
  tabItem(tabName = "resource_material",
          fluidRow(
            box(width = 3,
                title = "Browse material lists",
                shinyTree("material_tree")
                #rHandsontableOutput("hot_materials_list_ids")
            ),
            box(width = 9, solidHeader = TRUE, status = "warning",
                title = "Create new lists",
                fluidRow(
                  column(width = 4,
                         box(width = NULL,
                             radioButtons("mlist_choose_list_source", "Choose a source for the list:",
                                          choices = c("List", "Excel") ),
                             conditionalPanel(
                               "input.mlist_choose_list_source == 'Excel'",
                               shinyFilesButton('mlist_files', 'File selection', 'Please select a file', FALSE )
                               ,
                               verbatimTextOutput("mlist_fc")
                             ),
                             conditionalPanel(
                               "input.mlist_choose_list_source == 'List'",
                               selectInput("mlist_lists", label = "Please select list(s):",
                                           choices = list_material_lists())
                             )
                         )
                  ),
                  column(width = 4,
                         box(width = NULL,
                             selectInput("mlist_crop", "Choose a crop:",
                                         unique(get_crop_table()$crop_name) )
                         ),
                         box(width = NULL,
                             selectInput("mlist_year", "Choose a year:", 2000:2050 )
                         )
                  ),
                  column(width = 4,
                         box(width = NULL,
                             textInput("mlist_name", "Choose a list name:", "TEST123"),
                             actionButton("doListButton", "Create new material list!")

                         )
                  )

                )

                #rHandsontableOutput("hot_materials_list_defs")
            )
            # ,
            # box(width = 4, solidHeader = TRUE, status = "warning",
            #     title = "Create new lists - import materials lists",
            #     radioButtons("mlist_choose_list_source", "Choose a source for the list:",
            #                  choices = c("List", "Excel") ),
            #     #conditionalPanel()
            #     #hr(),
            #     conditionalPanel(
            #       "input.mlist_choose_list_source == 'Excel'",
            #       shinyFilesButton('mlist_files', 'File select', 'Please select a file', FALSE )
            #     )
            #
            # ),

          ),
          box( width = 12,
               title = "Plant materials:",
               #verbatimTextOutput('selTxt'),
               uiOutput('selectMList'),
               HTML("<br/>"),

               rHandsontableOutput("hot_materials", height = 300)
          )
  )
}
