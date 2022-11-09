#' Calculate Information score.
#'
#' @export
#'
calc_is <- function(...) UseMethod("calc_is")



#' @describeIn calc_is
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_is.default <- function(tp, fp, fn, n) {

  -log(((tp + fn) / n), base = 2) + log((tp / (tp + fp)), base = 2)

}



#' @describeIn calc_is
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_is.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_is(tp, fp, fn, n)

}



#' @describeIn calc_is
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_is.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_is(tbl)

}

