
#' Calculate Gwet AC1.
#'
#' @param pos Vector of positives in reference.
#' @param ppos Vector of positives in prediction.
#' @param otp Overall true positives.
#' @param on Number of samples.
#'
#' @export
#'
calc_gwet_ac1 <- function(pos, ppos, otp, on) {

  pc_ac1 <- calc_pc_ac1(pos, ppos, on)
  oacc <- calc_oacc(otp, on)

  calc_reliability(pc_ac1, oacc)

}
