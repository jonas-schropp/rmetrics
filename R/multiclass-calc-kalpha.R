#' Calculate Unweighted Krippendorff's Alpha.
#'
#' Krippendorff's alpha is a statistical measure of the agreement among a set
#' of raters who assign ratings to a set of items. It is commonly used in the
#' field of content analysis, where researchers use it to assess the reliability
#' of their coding schemes. It can be thought of as a generalization of the
#' concept of inter-rater reliability, which is a measure of how well two or
#' more raters agree on their ratings of a set of items. Krippendorff's alpha
#' allows for the calculation of reliability even when there are more than two
#' raters, and it can also be used with ordinal or continuous data, whereas
#' inter-rater reliability is typically only used with binary or nominal data.
#'
#' @param ... `r rox("dots")`
#'
#' @details
#' The alpha value ranges from 0 to 1, where 0 indicates no agreement among the
#' raters and 1 indicates perfect agreement. An alpha value of 0.7 or higher is
#' generally considered to indicate good agreement among the raters.
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
