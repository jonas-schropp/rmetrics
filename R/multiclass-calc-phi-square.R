#' Calculate Phi-squared.
#'
#' @export
#'
calc_phisq <- function(...) UseMethod("calc_phisq")



#' @describeIn calc_phisq
#'
#' @param tbl `r rox("tbl")`
#' @param ... Additional arguments passed on to `stats::chisq.test`.
#'
#' @export
#'
calc_phisq.table <- function(tbl, ...) {

  n <- sum(tbl)
  chisq.hat <- calc_chisq.table(tbl, correct = FALSE, ...)[1]

  chisq.hat / n

}



#' @describeIn calc_phisq
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ... Additional arguments passed on to `stats::chisq.test`.
#'
#' @export
#'
calc_phisq.data.frame <- function(
    data,
    prediction, reference,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_phisq(tbl, ...)

}
