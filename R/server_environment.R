server_environment <- function(input, output, session, values){

  #locs <- "data/sites/Master-list-trial-sites.xlsx"
  #locs <- readxl::read_excel(locs,1)
  locs <- get_site_table()
  names(locs) <- toupper(names(locs))

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

  output$rep_loc_docs <- renderUI({
    file_report = "reports/report_location.pdf"
    locs <- isolate({ locsInBounds()})

    pdf <-paste0(" <a href='",file_report,"'>PDF</a>")
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
  })

  loc_info <- eventReactive(input$map_marker_click, {
    event <- input$map_marker_click
    msg <- values[["map_msg"]]

    rec <- subset(locs,
                  LATD == as.numeric(event$lat) & LOND == as.numeric(event$lng))
    if(nrow(rec) != 1) return("No location selected.")
    #rec = rec[1:(ncol(rec))]
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

