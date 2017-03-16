
#' hidap_update
#'
#' @author Reinhard Simon
#'
#' @export
hidap_update <- function(){
  if (!brapi::ba_can_internet()) stop("No interent connection!")
  drat::addRepo("c5sire")
  utils::update.packages()
}
