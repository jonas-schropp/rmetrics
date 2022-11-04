
#' Calculate FNR macro.
#'
#' @param otp Overall true positives
#' @param on Overall N
#'
#' @export
#'
calc_fnr_micro <- function(otp, on) {

  1 - calc_oacc(otp, on)

}
