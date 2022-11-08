#' Calculate Diagnostic odds ratio
#'
#' @export
#'
calc_dor <- function(...) UseMethod("calc_dor")



#' @describeIn calc_dor
#'
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#'
#' @export
#'
calc_dor.default <- function(tp, fn, tn, fp) {

  plr <- calc_plr(tp, fn, fp, tn, F, 0)[1]
  nlr <- calc_nlr(fn, tp, tn, fp, F, 0)[1]

  plr / nlr

}



#' @describeIn calc_dor
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_dor.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_dor(tn, fp, tp, fn)

}



#' @describeIn calc_dor
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_dor.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_dor(tbl)

}
