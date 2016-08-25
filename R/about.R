#' about_ui
#'
#' @param title
#'
#' @return shiny tagList
#' @import shiny
#' @export
about_ui <- function(title = "About"){
  tagList(
  h2(title),

  img(src="about.png", width = "100%"),

  # br(),
  #br(),

  "HIDAP v1.0-preview [07/09/2016]",
  p(class = "text-muted", style="text-align:justify",
    paste("HIDAP is a tool designed to help breeders of clonal plants (likw potato and sweetpotato) carry out field trial planning, documentation, analysis and reporting.")
  ),
  #div(style="margin-right: auto;",img(src = "Logo1.png", width = "230"))
  tags$div(style = "color: #9b9691;float: right;", "International Potato Center (CIP)"),


  #br(),
  fluidRow(
    column(width = 2,
           img(src = "Logo1.png", width = "230")
    ),
    column(width = 1,
           img(src = "gt4sp.png", width = "100")
           ),
    column(width = 9)

  )

  # tags$div(style = "display:inline-block",
  #          img(src = "Logo1.png", width = "230"),
  #          img(src = "gt4sp.png", width = "100")
  # ),
  # br(),
  # br()
  )
}
