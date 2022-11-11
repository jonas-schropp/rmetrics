#' Calculate Calculate Overall Accuracy.
#'
#' @param ... `r rox("dots")`
#'
#' The proportion of overall true positives, regardless of class. Identical to micro-averaging TPR.
#'
#' @export
#'
calc_oacc <- function(...) UseMethod("calc_oacc")



#' @describeIn calc_oacc
#'
#' @param otp `r rox("otp")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_oacc.default <- function(otp, n, ...) {

  otp / n

}



#' @describeIn calc_oacc
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_oacc.table <- function(tbl, ...) {

  otp <- sum(diag(tbl))
  n <- sum(tbl)

  calc_oacc(otp, n, ...)

}



#' @describeIn calc_oacc
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_oacc.data.frame <- function(
    data,
    prediction, reference,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_oacc(tbl, ...)

}

