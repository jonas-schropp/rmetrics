
#' Calculate Kappa, unbiased Kappa or Kappa no Prevalence.
#'
#' @param tbl The contingency table.
#' @param unbiased Logical, should 'normal' or unbiased overall random accuracy be used.
#' @param prev TRUE for Kappa and unbiased Kappa, FALSE for Kappa no prevalence.
#'
#' @export
#'
calc_kappa <- function(tbl, unbiased = FALSE, prev = TRUE) {

  n <- sum(tbl)
  otp <- sum(diag(tbl))
  oacc <- calc_oacc(otp, n)

  if (prev) {
    oracc <- calc_oracc(tbl, unbiased)
    calc_reliability(oracc, oacc)
  } else {
    2 * oacc - 1
  }

}
