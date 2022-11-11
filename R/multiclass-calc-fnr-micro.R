#' Calculate FNR micro.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_fnr_micro <- function(...) UseMethod("calc_fnr_micro")



#' @describeIn calc_fnr_micro
#'
#' @param otp `r rox("otp")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_fnr_micro.default <- function(otp, n, ...) {

  1 - calc_oacc(otp, n)

}



#' @describeIn calc_fnr_micro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_fnr_micro.table <- function(tbl, ...) {

  otp <- sum(diag(tbl))
  n <- sum(tbl)

  calc_fnr_micro(otp, n)

}



#' @describeIn calc_fnr_micro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_fnr_micro.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_fnr_micro(tbl)

}

