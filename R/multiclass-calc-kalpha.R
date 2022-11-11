#' Calculate Unweighted Krippendorff's Alpha.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_kalpha <- function(...) UseMethod("calc_kalpha")



#' @describeIn calc_kalpha
#'
#' @param tbl `r rox("tbl")`
#' @param unbiased TRUE/FALSE. Should unbiased overall random accuracy be used?
#'
#' @export
#'
calc_kalpha.table <- function(tbl, unbiased = TRUE, ...) {

  n <- sum(tbl)
  otp <- sum(diag(tbl))

  oracc <- calc_oracc(tbl, unbiased = unbiased)
  oacc <- calc_oacc(otp, n)

  epsi <- 1 / (2 * n)
  acc <- (1 - epsi) * oacc + epsi

  calc_reliability(oracc, acc)

}



#' @describeIn calc_kalpha
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_kalpha.data.frame <- function(
    data,
    prediction, reference,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_kalpha(tbl)

}
