
#' Calculate Kappa, unbiased Kappa or Kappa no Prevalence.
#'
#' @param tbl The contingency table.
#' @param otp Overall number of true positives in the contingency table.
#' @param on Overall number of observations
#' @param unbiased Logical, should 'normal' or unbiased overall random accuracy be used.
#' @param prev TRUE for Kappa and unbiased Kappa, FALSE for Kappa no prevalence.
#'
#' @export
#'
calc_kappa <- function(tbl, otp, on, unbiased = FALSE, prev = TRUE) {

  oacc <- calc_oacc(otp, on)

  if (prev) {
    oracc <- calc_oracc(tbl, unbiased)
    calc_reliability(oracc, oacc)
  } else {
    2 * oacc - 1
  }

}
