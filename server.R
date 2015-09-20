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

#source("R/perspectives.R")
locs <- "data/sites/Master-list-trial-sites.xlsx"
locs <- readxl::read_excel(locs,1)


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
    if (!is.null(values[["hot_materials"]])) {
      post_material_table(values[["hot_materials"]])
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
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
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


  output$hot_materials = renderRHandsontable({
    tree <- input$material_tree
    if (is.null(tree)){
     list_name <- input$mlist_name
    } else {
     list_name <- get_selected_tree_node(tree)
    }
    #print(list_name)
    list_name = stringr::str_split("2001_TEST123", "_")[[1]][2]
    if (!is.null(input$hot_materials)) {
     DF_materials = hot_to_r(input$hot_materials)
    } else {
     #DF_materials = get_material_table()
     DF_materials <- get_material_table(input$mlist_crop,
                                        input$mlist_year,
                                        list_name)
    }
    print(DF_materials)
    setHot_materials(DF_materials)

    if(!is.null(DF_materials)){
      rhandsontable(DF_materials,   stretchH = "all") %>%
       hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }

  })

  output$material_tree <- renderTree({
    material_list_tree()
  })


  shinyFileChoose(input, 'mlist_files', session = session,
                  roots = volumes , filetypes = c('', 'xlsx')
                  )

  output$mlist_path <- renderPrint({
    out <- as.character(parseFilePaths(volumes, input$mlist_files)$datapath)
    setFile_materials(out)
    cat(out)
    out
    })

  output$selTxt <- renderText({
    tree <- input$material_tree
    if (is.null(tree)){
      "None"
    } else{
      paste(input$mlist_crop ,get_selected_tree_node(tree))
    }
  })

  mlist_text <- eventReactive(input$doListButton, {
    fn = values[["file_materials"]]
    if(input$mlist_choose_list_source == "List"){
      fn = file.path(fname_material_list, input$mlist_lists)
    } else {

    }
    #print(fn)
    import_list_from_prior(input$mlist_crop, input$mlist_year,
                           fn)

  })

  # output$selTxt <- renderText({
  #   mlist_text()
  # })

  #v <- reactiveValues(msg = "")


  locs$LATD <- as.numeric(locs$LATD)
  locs$LOND <- as.numeric(locs$LOND)
  lng1 <- min(locs$LOND)
  lng2 <- max(locs$LOND)
  lat1 <- min(locs$LATD)
  lat2 <- max(locs$LATD)


  nvar <- abs(round(rnorm(length(locs$LOND))*100, 0)) + 10
  locs <- cbind(locs, nvar)

  observeEvent(input$map_env_marker_click, {
    setMap_msg(input$map_env_marker_click)

  })

#   output$hist_nvar <- renderPlot({
#     data <- locsInBounds()$nvar
#     x <- 1:length(data)
#     if(length(x) != length(data)) return(NULL)
#     #plot(x, data)
#     hist(data)
#   })


  output$rep_loc <- renderUI({

    input$locs_report_button

    locs <- isolate({ locsInBounds()})
    n = nrow(locsInBounds())
    if(n<1) return("no locations in view!")
    fn <- rmarkdown::render("reports/report_location.Rmd",
                            #output_format = "all",
                            output_dir = "www/reports/",
                            params = list(
                              locs = locs))

    html <- readLines("www/reports/report_location.html")
    HTML(html)


  })

#   output$rep_loc_pdf <- downloadHandler(
#     filename = function() {
#       paste('reports/report_location.pdf', sep='')
#     },
#     content = function(con) {
#       readBin("reports/report_location.pdf", "raw")
#     }
#   )
  output$rep_loc_docs <- renderUI({
    file_report = "reports/report_location.pdf"
    #HTML(a(href="reports/report_location.pdf"))
    #if(file.exists(file_report)) {
    #input$locs_report_button

    locs <- isolate({ locsInBounds()})

    pdf <-paste0(" <a href='",file_report,"'>PDF</a>")
    #}
    file_report = "reports/report_location.docx"
    if(file.exists(file_report)) {
      docx <-paste0("<a href='",file_report,"'>DOCX</a>")
    }
    HTML(paste(pdf, docx))
  })

    output$dot_yield <- renderPlot({
    data <- locsInBounds()$ELEV
    n = length(data)
    data <- as.numeric(data)
    if(n < 1) return("no data")
    hist(data, main = "Elevation", xlim = c(0,3600))
    #yvar <- rpois(10, db)
    #nmsv <- paste("Clon", 10:19)
    #dta <- as.data.frame(cbind(nmsv, yvar))

    #lattice::dotplot(nmsv ~ yvar, data = dta, horizontal=TRUE)
  })

  loc_info <- eventReactive(input$map_marker_click, {
    event <- input$map_marker_click
    msg <- values[["map_msg"]]

    rec <- subset(locs,
                  LATD == as.numeric(event$lat) & LOND == as.numeric(event$lng))
    if(nrow(rec) != 1) return("No location selected.")
    rec = rec[1:(ncol(rec)-3)]
    paste(names(rec),": ", rec, "<br/>", sep="")
  }, ignoreNULL = FALSE)

  output$site_desc <- renderUI ({
    HTML(loc_info())
  })

  locsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(locs[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    #print(bounds)
    subset(locs,
           LATD >= latRng[1] & LATD <= latRng[2] &
             LOND >= lngRng[1] & LOND <= lngRng[2])
  })


  output$map <- renderLeaflet({
    m = leaflet(width = "50%") %>% addTiles()
    m  # a map with the default OSM tile layer

    m = m %>% fitBounds(lng1,lat1, lng2, lat2)
    m

    m %>% addMarkers(locs$LOND, locs$LATD,popup=locs$FULLN,
                     clusterOptions = markerClusterOptions())
  })

}

