#' about
#'
#' @param title character
#'
#' @return shiny tagList
#' @import shiny
#' @export
about <- function(title = "About"){
  #hidap_release = paste0("HIDAP preview 1.0 (build: ", packageVersion("hidap"), ") [", Sys.Date(), "]")

  about_hidap = "www/about_hidap.txt"

  shinydashboard::tabItem(tabName = "dashboard",
          h2("Highly Interactive Data Analysis Platform"),

          #br(),
          br(),
          #img(src="potato.jpg", width = "100%"),
          img(src="about.png", width = "100%"),

          br(),
          br(),

          "HIDAP Preview [19/09/2016]",
          p(class = "text-muted", style="text-align:justify",
            #paste("HIDAP is a tool designed to help plant breeders of clonal crops like potato and sweetpotato to carry out field trial planning, documentation, analysis and reporting")
            shiny::includeHTML(about_hidap)

          ),


          #br(),
          #br(),

          fluidRow(
            shinydashboard::box(
              width = 2, style="background-color = #fff", height = "128px",
              solidHeader = TRUE,
              br(),
              div(img(src="CIP_Logo_300px_RGB.png", width = "150px"), style="text-align: center;")
            ),
            shinydashboard::box(
              width = 2, style="background-color = #fff", height = "128px",
              solidHeader = TRUE,
              div(img(src="gt4sp.png", height = "108px"), style="text-align: center;")
            ),
            shinydashboard::box(
              width = 2, style="background-color = #fff", height = "128px",
              solidHeader = TRUE,
              br(),
              div(img(src="usaid.png", width = "150px"), style="text-align: center;")
            ),
            shinydashboard::box(
              width = 2, style="background-color = #fff", height = "128px",
              solidHeader = TRUE,
              div(img(src="sasha.png"), style="text-align: center;")
            ),
            shinydashboard::box(
              width = 2, style="background-color = #fff", height = "128px",
              solidHeader = TRUE,
              br(),
              div(img(src="rtb.png", width = "150px"), style="text-align: center;")
            )
          ),

          br(),
          #br(),
          br()
  )
}
