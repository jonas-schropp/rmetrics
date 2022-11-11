#' Calculate Hamming Loss.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_hamming <- function(...) UseMethod("calc_hamming")



#' @describeIn calc_hamming
#'
#' @param otp `r rox("otp")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_hamming.default <- function(otp, n, ...) {

  (1 / n) * (n - otp)

}



#' @describeIn calc_hamming
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_hamming.table <- function(tbl, ...) {

  otp <- sum(diag(tbl))
  n <- sum(tbl)

  calc_hamming(otp, n)

}



#' @describeIn calc_hamming
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_hamming.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  otp <- sum(data[[1]] == data[[2]])
  n <- nrow(data)

  calc_hamming(otp, n)

}
