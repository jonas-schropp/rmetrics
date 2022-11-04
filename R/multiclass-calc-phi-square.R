
#' Calculate Phi-squared.
#'
#' @param tbl confusion matrix
#' @param on Total number of observations
#'
#' @export
#'
calc_phi_square <- function(tbl, on) {

  calc_chisq(tbl) / on

}
