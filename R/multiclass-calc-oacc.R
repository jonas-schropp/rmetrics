#' Calculate Calculate Overall accuracy (oacc).
#'
#' Also called TPR Micro.
#'
#' @param otp Overall number of true positives in the contingency table.
#' @param on Overall number of observations
#'
#' @export
#'
calc_oacc <- function(otp, on) {

  otp / on

}
