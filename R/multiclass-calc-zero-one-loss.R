#' Calculate Zero-One Loss.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_zero_one_loss <- function(...) UseMethod("calc_zero_one_loss")



#' @describeIn calc_zero_one_loss
#'
#' @param otp `r rox("otp")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_zero_one_loss.default <- function(otp, n, ...) {

  n - otp

}


#' @describeIn calc_zero_one_loss
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_zero_one_loss.table <- function(tbl, ...) {

  otp <- sum(diag(tbl))
  n <- sum(tbl)

  n - otp

}



#' @describeIn calc_zero_one_loss
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_zero_one_loss.data.frame <- function(
    data,
    prediction, reference,
    ...
) {

  data <- data[, c(prediction, reference)]

  otp <- sum(data[[1]] == data[[2]])
  n <- length(data[[1]])

  n - otp

}
