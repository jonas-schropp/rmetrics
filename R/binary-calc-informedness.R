#' Calculate Informedness
#'
#' @export
#'
calc_informedness <- function(...) UseMethod("calc_informedness")



#' @describeIn calc_informedness
#'
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#'
#' @export
#'
calc_informedness.default <- function(tp, fn, tn, fp) {

  tpr <- calc_tpr(tp, fn, F, 0)[1]
  tnr <- calc_tnr(tn, fp, F, 0)[1]

  tpr + tnr - 1

}



#' @describeIn calc_informedness
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_informedness.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_informedness(tp, fn, tn, fp)

}



#' @describeIn calc_informedness
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_informedness.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_informedness(tbl)

}
