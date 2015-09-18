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
                  box(width = 3,
                    selectInput("mlist_crop", "Choose a crop:",
                                unique(get_crop_table()$crop_name) )
                  ),
                  box(width = 3,
                    selectInput("mlist_year", "Choose a year:", 2000:2050 )
                  ),
                  box(width = 3,
                    textInput("mlist_name", "Choose a list name:", "TEST123")
                  ),
                  box(width = 3,
                    radioButtons("mlist_choose_list_source", "Choose a source for the list:",
                                 choices = c("List", "Excel") ),
                    conditionalPanel(
                      "input.mlist_choose_list_source == 'Excel'",
                      shinyFilesButton('mlist_files', 'File select', 'Please select a file', FALSE )
                    ),
                    conditionalPanel(
                      "input.mlist_choose_list_source == 'List'",
                      selectInput("mlist_lists", label = "Please select list(s):",
                                     choices = list_material_lists())
                    )
                  ),
                  box(width = 3,
                    verbatimTextOutput('mlist_path'),

                    actionButton("doListButton", "Create new material list!")
                  )

                )

                #rHandsontableOutput("hot_materials_list_defs")
            ),
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
            box( width = 12,
              title = "Plant materials:",
              verbatimTextOutput('selTxt'),

              rHandsontableOutput("hot_materials")
            )
          )
  )
}
