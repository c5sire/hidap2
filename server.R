
library(shiny)
library(rhandsontable)
#library(shinyTree)
library(shinyFiles)
#library(leaflet)
library(rmarkdown)

source("R/utils.R")
source("R/utils_fieldbook.R")
source("R/server_environment.R")


shinyServer <- function(input, output, session) {

  values = shiny::reactiveValues()

  fbsites::server_site(input, output, session, values = values)
  fbcrops::server_crop(input, output, session, values = values)
  fbprogram::server_program(input, output, session, values = values)
  fbprstages::server_program_stages(input, output, session, values = values)
  fbmaterials::server_material_list(input, output, session, values = values)
  cropont::server_dictionary(input, output, session, values = values)
  fbmodule::server_module(input, output, session, values = values)

  setMap_msg = function(x) values[["map_msg"]] = x


  output$fb_fieldmap_check <- d3heatmap::renderD3heatmap({

    if (!is.null(input[["phenotype_fb_choice"]])) {
      DF <- fbmaterials::get_fieldbook_data(  input[["phenotype_fb_choice"]])
      ci = input$hotFieldbook_select$select$c
      #print(ci)
      trt = names(DF)[ncol(DF)]
      if (!is.null(ci)) trt = names(DF)[ci]
      #print(trt)
      fm <- fbmaterials::fb_to_map(DF, variable = trt)
      amap = fm[["map"]]
      anot = fm[["notes"]]
      # print(head(amap))
      # print(head(anot))
      d3heatmap::d3heatmap(x = amap,
                             cellnote = anot,
                                 colors = "Blues",
                           Rowv = FALSE, Colv = FALSE,
                           dendrogram = "none")
    }

  })



  # output$module_list <- renderUI({
  #   selectInput("module_crop_module", "Select a module:",
  #               choices = get_crop_modules(input$module_crop) )
  # })

  # output$module_var_list <- renderText({
  #   fp <- file.path(fname_module, "PTBM")
  #   load(fp)
  #   x <- paste(list_variables[,2],":", list_variables[,1])
  #   paste(x, collapse = ",\n ")
  # })

  output$fieldbook_list <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    selectInput("phenotype_fb_choice", "Select a fieldbook:",
                choices = fbmaterials::get_fieldbook_list(input$fb_analysis_crop,
                                                        TRUE))
  })

  output$hotFieldbook <- renderRHandsontable({
    #try({
    #if(exists(input[["fb_analysis_crop"]])) {
    #exists_fb <- fbmaterials::exists_fieldbook(input[["fieldbook_list"]])
    #if(!exists_fb) return("")
    #print(" check HoT fb")
    try({
      #if(!is.null(input[["phenotype_fb_choice"]])) {
        DF = fbmaterials::get_fieldbook_data(
          input$phenotype_fb_choice)

        if(!is.null(DF)){
          #setHot_sites(DF)
          rhandsontable(DF,
                        selectCallback = TRUE) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            hot_cols( fixedColumnsLeft = 6)
        }

      #}

    })
    #}
    #}, silent = TRUE)
   } )


  output$fb_def_reps <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    if(!is.null(input[["phenotype_fb_choice"]])) {
      tf <- fbmaterials::get_trial_factors(input$phenotype_fb_choice)
      if(length(tf) > 0) {
        ti <- tf[stringr::str_sub(tf, 1,1 ) == "R"]
        return(selectInput("def_rep","Define replication:", tf, ti))
      }
    }
    ""
  })

  output$fb_def_block <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    if(!is.null(input[["phenotype_fb_choice"]])) {
      tf <- fbmaterials::get_trial_factors(input$phenotype_fb_choice)
      if(length(tf) > 0) {
        ti <- tf[stringr::str_sub(tf, 1,1 ) == "B"]
        tf <- c(tf, "")
        return(selectInput("def_block","Define block:", tf, ti))
      }
    }
    ""
  })

  output$fb_def_genotype <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    if(!is.null(input[["phenotype_fb_choice"]])) {
      tf <- fbmaterials::get_trial_factors(input$phenotype_fb_choice)
      if(length(tf) > 0) {
        ti <- tf[stringr::str_sub(tf, 1,1 ) %in% c("G", "I", "C")]
        return(selectInput("def_genotype","Define genotype:", tf, ti))
      }
    }
    ""
  })

  output$fb_def_variables <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    if(!is.null(input[["phenotype_fb_choice"]])) {
      tf <- fbmaterials::get_trial_variables(input$phenotype_fb_choice)
      if(length(tf) > 0) {
        #ti <- tf[stringr::str_sub(tf, 1,1 ) %in% c("G", "I", "C")]
        chc <- as.list(tf)
        names(chc) = tf
        selected = names(chc)[(length(chc) - 3):length(chc)]
        #print(chc)
        return(selectizeInput("def_variables",choices = chc,
                              selected = selected,
                              label = "Variables",
                              multiple = TRUE))
      }
    }
    ""
  })



  shiny::observeEvent(input$butDoPhAnalysis, ({
    #if(exists(input[["fb_analysis_crop"]])) {
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    #print("1")
    if(!exists_fb) return("")
    #print("2")
    #if(is.null(input$horFieldbook)) return("")
    #print("3")
    if (!is.null(input[["phenotype_fb_choice"]])) {
     # print("4")


      DF <- fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      #print("4A")
      # y <- input$hotFieldbook_select$select$c
      # if(is.null(y)) return(HTML(""))
      # #print(y)
      # y <- names(DF)[y]

      y <- input$def_variables
      #print("1")
      if(input$fb_analysis == "descriptive"){
        report = paste0("report_",input$fb_analysis,".Rmd")
        report_dir = file.path("inst", "rmd")
      }
      if(input$fb_analysis == "aov"){

        report =  "rcbd2_withchild.Rmd"
        report_dir = system.file("rmd", package = "pepa")

        #y = y[1]
      }
      wd = getwd()
      result_dir  = file.path(wd, "www", "reports")
      print(attr(DF, "meta"))
      # print(report_dir)
      # print(wd)
      # print(result_dir)
      author =  paste0(Sys.getenv("USERNAME"), " using HIDAP")
      withProgress(message = "Creating report ...",
                   detail = "This may take a while ...", value = 0,{
        try({
          devtools::in_dir(report_dir, {
            print("X")
            rmarkdown::render(report,
                              output_format = c("pdf_document", "word_document",
                                                "html_document"),
                              output_dir = file.path(wd, "www"),
                              params = list(
                                meta = attr(DF, "meta"),
                                data = DF,
                                rep  = input$def_rep,
                                treat = input$def_genotype,
                                trait = y,
                                maxp = 0.1,
                                author = author))
            print("Y")
          }) # in_dir
          incProgress(1/3)
        }) # try

      try({
         report_html = stringr::str_replace(report, ".Rmd", ".html")
      })
      output$fb_report <- renderUI("")
      report = file.path(wd, "www", report_html)
      print(report)
      html <- readLines(report)
      incProgress(3/3)
      })
      output$fb_report <- renderUI(HTML(html))

    }
  })

  )

  output$fb_fieldmap_title <- renderText({
    out  = ""
  if (!is.null(input[["phenotype_fb_choice"]])) {
    DF <- fbmaterials::get_fieldbook_data(  input[["phenotype_fb_choice"]])
    reps = unlist(input[["def_rep"]])
      ci = input$hotFieldbook_select$select$c
      trt = names(DF)[ncol(DF)]
      if (!is.null(ci)) trt = names(DF)[ci]
    out = paste("Displaying spatial variation of trait variable:",  trt)
    if(length(unique(DF[, reps])) <= 1) {
      out = "Only one replication."
    }
  }
    HTML(out)
  })

  output$fb_fieldbook_title <- renderText({
    out  = ""
    if (!is.null(input[["phenotype_fb_choice"]])) {
      out = input$phenotype_fb_choice
    }
    HTML(out)
  })







  # output$fb_report <- renderUI({
  #   if(!is.null(input[["phenotype_fb_choice"]])) {
  #   DF = fbmaterials::get_fieldbook_data(
  #     input$fb_phenotype_fb_choice)
  #
  #   y <- input$hotFieldbook_select$select$c
  #   if(is.null(y)) return(HTML(""))
  #   #print(y)
  #   y <- names(DF)[y]
  #
  #   report = paste0("reports/report_",input$fb_analysis,".Rmd")
  #
  #   fn <- rmarkdown::render(report,
  #                           #output_format = "all",
  #                           output_dir = "www/reports/",
  #                           params = list(
  #                             fieldbook = DF,
  #                             independent = input$fb_def_geno,
  #                             dependent = y))
  #
  #   report = paste0("www/reports/report_",input$fb_analysis,".html")
  #   html <- readLines(report)
  #   HTML(html)
  #   }
  # })



  setHot_cross_marker = function(x) values[["hot_cross_marker"]] = x

  output$hot_cross_marker = rhandsontable::renderRHandsontable({
    shiny::withProgress(message = 'Loading table', {
      #list_name <- input$module_name
      #print(input$module_crop)
      DF_cross_marker <- fbqtl::get_cross_marker_table(crop = "potato",
                                                       name = "data.loc.rds" )
      #print(DF_cross_marker)
      if(!is.null(DF_cross_marker)){
        setHot_cross_marker(DF_cross_marker)
        rh <- rhandsontable::rhandsontable(DF_cross_marker,   stretchH = "all")
        rhandsontable::hot_table(rh, highlightCol = TRUE, highlightRow = TRUE)
      } else {
        NULL
      }
    })
  })


  library(qtlcharts)
  data(geneExpr)
  data(grav)
  library(qtl)


  output$qtl_output = qtlcharts::iplotCorr_render({
    iplotCorr(geneExpr$expr, geneExpr$genotype, reorder=TRUE,
              chartOpts=list(cortitle="Correlation matrix",
                             scattitle="Scatterplot"))
  })

  output$lod_output = qtlcharts::iplotScanone_render({
    data(hyper)
    hyper <- calc.genoprob(hyper, step=1)
    out <- scanone(hyper)

    # iplotScanone with no effects
    iplotScanone(out, chr=c(1, 4, 6, 7, 15))


    # iplotScanone with CIs
    iplotScanone(out, hyper, chr=c(1, 4, 6, 7, 15))

  })

  output$qtl_map_output = qtlcharts::iplotScanone_render({
    data(hyper)
    map <- pull.map(hyper)[1:15]

    iplotMap(map, shift=TRUE)

  })

  output$qtl_time_output = qtlcharts::iplotMScanone_render({
    data(grav)
    library(qtl)
    grav <- calc.genoprob(grav, step=1)
    grav <- reduce2grid(grav)

    # we're going to subset the phenotypes
    phecol <- seq(1, nphe(grav), by=5)

    # the times were saved as an attributed
    times <- attr(grav, "time")[phecol]

    # genome scan
    out <- scanone(grav, phe=phecol, method="hk")


    # plot with qualitative labels on y-axis
    iplotMScanone(out)

  })

  output$rf_output = qtlcharts::iplotRF_render({
    data(fake.f2)
    fake.f2 <- est.rf(fake.f2)
    iplotRF(fake.f2)
  })



  server_environment(input, output, session, values)

}

