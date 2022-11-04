
#' Calculate Zero-One loss.
#'
#' @param otp Overall true positives
#' @param on Overall N
#'
#' @export
#'
calc_zero_one_loss <- function(otp, on) {

  on - otp

}
