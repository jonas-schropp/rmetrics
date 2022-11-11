#' Calculate Pearson's C.
#'
#' @export
#'
calc_pearson_c <- function(...) UseMethod("calc_pearson_c")



#' @describeIn calc_pearson_c
#'
#' @param tbl `r rox("tbl")`
#' @param ... Additional arguments passed on to `stats::chisq.test`. Not used.
#'
#' @export
#'
calc_pearson_c.table <- function(tbl, ...) {

  n <- sum(tbl)
  chisq.hat <- calc_chisq.table(tbl, correct = FALSE, ...)[1]

  sqrt(chisq.hat / (n + chisq.hat))

}



#' @describeIn calc_pearson_c
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ... Additional arguments passed on to `stats::chisq.test`. Not used.
#'
#' @export
#'
calc_pearson_c.data.frame <- function(
    data,
    prediction, reference,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_pearson_c(tbl, ...)

}
