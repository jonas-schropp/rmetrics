#' Calculate Individual classification success index.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_icsi <- function(...) UseMethod("calc_icsi")



#' @describeIn calc_icsi
#'
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param fp `r rox("fp")`
#'
#' @export
#'
calc_icsi.default <- function(tp, fn, fp, ...) {

  tpr <- calc_tpr(tp, fn, FALSE, 0)[1]
  ppv <- calc_precision(tp, fp, FALSE, 0)[1]

  tpr + ppv - 1

}



#' @describeIn calc_icsi
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_icsi.table <- function(tbl, ...) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_icsi(tp, fn, fp)

}



#' @describeIn calc_icsi
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_icsi.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_icsi(tbl)

}
