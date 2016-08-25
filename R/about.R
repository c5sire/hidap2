#' about
#'
#' @param title
#'
#' @return shiny tagList
#' @import shiny
#' @export
about <- function(title = "About"){
  hidap_release = paste0("HIDAP ", packageVersion("hidap"), " [", Sys.Date(), "]")

  # about_sv <- function(input, output, session) ({
  #   eventReactive(input$about_update, {
  #     devtools::update_packages()
  #   })
  # })

  out = tagList(
  h2(title),

  img(src="about.png", width = "100%"),
  hidap_release,
  p(class = "text-muted", style="text-align:justify",
    paste("HIDAP is a tool designed to help breeders of clonal plants (likw potato and sweetpotato) carry out field trial planning, documentation, analysis and reporting.")
  ),
  fluidRow(
    column(width = 2, img(src = "Logo1.png", width = "230") ),
    column(width = 2, img(src = "rtb.png", width = "230")   ),
    column(width = 2, img(src = "gt4sp.png", width = "100") ),
    column(width = 2, img(src = "sasha.png", width = "230") ),
    column(width = 2, img(src = "usaid.png", width = "230") ),
    column(width = 2)

  ),
  br()
  # ,
  # actionButton("about_update", "Check for updates.", icon = icon("cloud-download"))
  )

  out
}
