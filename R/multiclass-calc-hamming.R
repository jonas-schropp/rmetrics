
#' Calculate Hamming loss.
#'
#' @param otp Overall true positives
#' @param on Onverall N
#'
#' @export
#'
calc_hamming <- function(otp, on) {

  (1 / on) * (on - otp)

}
