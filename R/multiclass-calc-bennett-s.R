
#' Calculate Bennett's S.
#'
#' @param tbl input table
#' @param otp Overall true positives.
#' @param on Number of samples.
#'
#' @export
#'
calc_bennett_s <- function(tbl, otp, on) {

  pc_s <- calc_pc_s(tbl)
  oacc <- calc_oacc(otp, on)

  calc_reliability(pc_s, oacc)

}
