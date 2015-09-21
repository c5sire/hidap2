library(shiny)
library(rhandsontable)
library(shinyTree)
library(shinyFiles)
library(leaflet)
library(rmarkdown)

source("R/global.R") # global needs to be loaded first
source("R/utils.R")
source("R/utils_crop.R")
source("R/utils_program.R")
source("R/utils_program_stage.R")
source("R/utils_material.R")
source("R/utils_site.R")
source("R/utils_dictionary.R")

source("R/server_environment.R")

shinyServer <- function(input, output, session) {

  volumes <- getVolumes()

  values = reactiveValues()
  setHot_crops = function(x) values[["hot_crops"]] = x
  setHot_programs = function(x) values[["hot_programs"]] = x
  setHot_program_stages = function(x) values[["hot_program_stages"]] = x
  setHot_materials = function(x) values[["hot_materials"]] = x
  setFile_materials = function(x) values[["file_materials"]] = x
  setMap_msg = function(x) values[["map_msg"]] = x
  setMat_list_sel = function(x) values[["mat_list_sel"]]
  setHot_sites = function(x) values[["hot_sites"]] = x
  #setHot_sites = function(x) values[["hot_sites"]] = x

  observe({
    input$saveBtn
    if (!is.null(values[["hot_crops"]])) {
      post_crop_table(values[["hot_crops"]])
    }
    if (!is.null(values[["hot_programs"]])) {
      post_program_table(values[["hot_programs"]])
    }
    if (!is.null(values[["hot_program_stages"]])) {
      post_program_stage_table(values[["hot_program_stages"]])
    }
    if (!is.null(values[["hot_sites"]])) {
      post_site_table(values[["hot_sites"]])
    }

  })

  output$hot_crops = renderRHandsontable({
    if (!is.null(input$hot_crops)) {
      DF = hot_to_r(input$hot_crops)
    } else {
      DF = get_crop_table()
    }

    setHot_crops(DF)
    rhandsontable(DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE, limit = 2)
      # %>% hot_context_menu(allowRowEdit = FALSE)
  })

  output$hot_programs = renderRHandsontable({
    if (!is.null(input$hot_programs)) {
      DF_programs = hot_to_r(input$hot_programs)
    } else {
      DF_programs = get_program_table()
    }

    setHot_programs(DF_programs)
    rhandsontable(DF_programs,   stretchH = "all") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  output$hot_program_stages = renderRHandsontable({
    if (!is.null(input$hot_program_stages)) {
      DF_program_stages = hot_to_r(input$hot_program_stages)
    } else {
      DF_program_stages = get_program_stage_table()
    }

    setHot_program_stages(DF_program_stages)
    rhandsontable(DF_program_stages,   stretchH = "all") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  rv_fp_ml <- reactive({
    roots = c(wd = ".")
    fp <- parseFilePaths( roots, input$mlist_files)$datapath
    fp <- stringr::str_replace(fp, "NA", "")
   fp
  })

  output$mlist_fc <- renderText({
    rv_fp_ml()
  })

  # obs_ml <- observe({
  #   roots = c(wd = ".")
  #   fp <- parseFilePaths( roots, input$mlist_files)$datapath
  #   fp
  # })




  # output$material_tree <- renderTree({
  #   material_list_tree()
  # })


  shinyFileChoose(input, 'mlist_files', session = session,
                  roots = volumes , filetypes = c('', 'xlsx')
                  )


  output$selectMList <- renderUI({
    lbl <-paste0("Save: ",input$mlist_crop,"/",
                 input$mlist_year,"_",input$mlist_name)

    actionButton("saveMListButton", lbl)

  })

  observeEvent(input$doListButton, {
    if(input$mlist_choose_list_source == "List"){
      fn = file.path(fname_material_list, input$mlist_lists)
    } else {
      fn = rv_fp_ml()
    }

    import_list_from_prior(input$mlist_crop, input$mlist_year, input$mlist_name,
                           fn)

  })

  observeEvent(input$saveMListButton, {
    table_materials = hot_to_r(input$hot_materials)
    if(!is.null(table_materials)){
      post_material_table(table_materials,
        input$mlist_crop, input$mlist_year, input$mlist_name)

      fn <- file.path(fname_materials,input$mlist_crop,
                      paste0(input$mlist_year,"_",input$mlist_name))
     }
  })


  #mlist_data <- reactiveFileReader(1000, session, rv_fp_ml(), load)


  output$hot_materials = renderRHandsontable({
    list_name <- input$mlist_name
    DF_materials <- get_material_table(input$mlist_crop,
                                       input$mlist_year,
                                       list_name)
    setHot_materials(DF_materials)

    if(!is.null(DF_materials)){
      rhandsontable(DF_materials,   stretchH = "all", limit = 2) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }

  })

  output$downloadMaterialListData <- downloadHandler(
    filename = function() {
      paste('germplasm_list-', input$mlist_crop,"_", input$mlist_year,"_",
            input$mlist_name, "_",
            Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv( values[["hot_materials"]], con)
    }
  )


  output$hot_sites = renderRHandsontable({
    if (!is.null(input$hot_sites)) {
      DF = hot_to_r(input$hot_sites)
    } else {
      DF = get_site_table()
    }

    setHot_sites(DF)
    rhandsontable(DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
    # %>% hot_context_menu(allowRowEdit = FALSE)
  })

  output$hot_dictionary <- renderRHandsontable({
    if (!is.null(input$hot_dictionary)) {
      DF = hot_to_r(input$hot_dictionary)
    } else {
      DF = get_dictionary_table(input$dictionary_crop)
    }
    if(!is.null(DF)){
      #setHot_sites(DF)
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE, limit = 2)
    }
  })

  output$module_list <- renderUI({
    selectInput("module_crop_module", "Select a module:",
                choices = get_crop_modules(input$module_crop) )
  })

  output$module_var_list <- renderText({
    fp <- file.path(fname_module, "PTBM")
    load(fp)
    x <- paste(list_variables[,2],":", list_variables[,1])
    #paste(x, collapse = ",\n ")
  })

  output$fieldbook_list <- renderUI({
    selectInput("phenotype_fb_choice", "Select a fieldbook:",
                choices = get_fieldbook_list(input$fb_analysis_crop))
  })

  output$hotFieldbook <- renderRHandsontable({
      DF = get_fieldbook_table(
              input$fb_analysis_crop,
              input$phenotype_fb_choice)

    if(!is.null(DF)){
      #setHot_sites(DF)
      rhandsontable(DF,
                    selectCallback = TRUE) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols( fixedColumnsLeft = 3)
    }
  })


  output$fb_report <- renderUI({
    DF = get_fieldbook_table(
      input$fb_analysis_crop,
      input$phenotype_fb_choice)

    y <- input$hotFieldbook_select$select$c
    if(is.null(y)) return(HTML(""))
    print(y)
    y <- names(DF)[y]
    #print(y)


    report = paste0("reports/report_",input$fb_analysis,".Rmd")

    fn <- rmarkdown::render(report,
                            #output_format = "all",
                            output_dir = "www/reports/",
                            params = list(
                              fieldbook = DF,
                              independent = input$fb_def_geno,
                              dependent = y))

    report = paste0("www/reports/report_",input$fb_analysis,".html")
    html <- readLines(report)
    HTML(html)


  })


  server_environment(input, output, session, values)

}

