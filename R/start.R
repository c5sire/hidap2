#' Start HIDAP
#'
#' Starting HIDAP (a shiny application)
#'
#' @author Reinhard Simon
#' @export
start <- function(){
  fp = system.file("hd", package = "hidap")
  shiny::runApp(fp)
}
