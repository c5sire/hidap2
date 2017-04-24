
#' hidap_update
#'
#' @author Reinhard Simon
#'
#' @export
hidap_update <- function(){

    brapi::ba_can_internet()
    drat::addRepo("c5sire")
    utils::update.packages()

}
