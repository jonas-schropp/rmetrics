
#' Calculate Unweighted Krippendorff's alpha (kalpha).
#'
#' @param tbl The contingency table.
#' @param unbiased TRUE/FALSE. Should unbiased overall random accuracy be used?
#'
#' @export
#'
calc_kalpha <- function(tbl, unbiased = TRUE) {

  n <- sum(tbl)
  otp <- sum(diag(tbl))

  oracc <- calc_oracc(tbl, unbiased = unbiased)
  oacc <- calc_oacc(otp, n)

  epsi <- 1 / (2 * n)
  acc <- (1 - epsi) * oacc + epsi

  calc_reliability(oracc, acc)

}
