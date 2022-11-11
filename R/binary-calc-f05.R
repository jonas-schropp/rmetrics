#' Calculate F0.5 Score
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_f05 <- function(...) UseMethod("calc_f05")



#' @describeIn calc_f05
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_f05.default <- function(tp, fp, fn, ...) {

  calc_f(tp, fp, fn, 0.5)

}



#' @describeIn calc_f05
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_f05.table <- function(tbl, ...) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_f(tp, fp, fn, 0.5)

}



#' @describeIn calc_f05
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_f05.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_f05(tbl)

}
