#' Calculate Markedness.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_markedness <- function(...) UseMethod("calc_markedness")



#' @describeIn calc_markedness
#'
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#'
#' @export
#'
calc_markedness.default <- function(tp, fn, tn, fp, ...) {

  ppv <- calc_precision(tp, fp, F, 0)[1]
  npv <- calc_npv(tn, fn, F, 0)[1]

  ppv + npv - 1

}



#' @describeIn calc_markedness
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_markedness.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_markedness.default(tp, fn, tn, fp)

}



#' @describeIn calc_markedness
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_markedness.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_markedness.table(tbl, incr)

}
