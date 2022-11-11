#' Calculate F2 Score
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_f2 <- function(...) UseMethod("calc_f2")



#' @describeIn calc_f2
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_f2.default <- function(tp, fp, fn, ...) {

  calc_f(tp, fp, fn, 2)

}



#' @describeIn calc_f2
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_f2.table <- function(tbl, ...) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_f(tp, fp, fn, 2)

}



#' @describeIn calc_f2
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_f2.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_f2(tbl)

}
