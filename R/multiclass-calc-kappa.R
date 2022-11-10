#' Calculate Kappa, unbiased Kappa or Kappa no Prevalence.
#'
#' @export
#'
calc_kappa <- function(...) UseMethod("calc_kappa")



#' @describeIn calc_kappa
#'
#' @param tbl `r rox("tbl")`
#' @param unbiased Should 'normal' or unbiased overall random accuracy be used. FALSE by default.
#' @param prev TRUE for Kappa and unbiased Kappa, FALSE for Kappa no prevalence. TRUE by default.
#'
#' @export
#'
calc_kappa.table <- function(
    tbl,
    unbiased = FALSE,
    prev = TRUE
    ) {

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



#' @describeIn calc_kappa
#'
#' @param data `r rox("data")`
#' @param unbiased Logical, should 'normal' or unbiased overall random accuracy be used.
#' @param prev TRUE for Kappa and unbiased Kappa, FALSE for Kappa no prevalence.
#'
#' @export
#'
calc_kappa.data.frame <- function(
    data,
    unbiased = FALSE,
    prev = TRUE
    ) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_kappa(tbl)

}
