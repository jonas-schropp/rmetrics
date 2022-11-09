#' Calculate Index of Balanced Accuracy.
#'
#' @export
#'
calc_iba <- function(...) UseMethod("calc_iba")



#' @describeIn calc_iba
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param alpha Weight for TPR - TNR. By default 1.
#'
#' @export
#'
calc_iba.default <- function(tn, fp, tp, fn, alpha = 1) {

  tnr <- calc_tnr(tn, fp, FALSE, 0)[1]
  tpr <- calc_tpr(tp, fn, FALSE, 0)[1]

  (1 + alpha * (tpr - tnr)) * tpr * tnr

}



#' @describeIn calc_iba
#'
#' @param tbl `r rox("tbl")`
#' @param alpha Weight for TPR - TNR. By default 1.
#'
#' @export
#'
calc_iba.table <- function(tbl, alpha = 1) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_iba(tn, fp, tp, fn, alpha)

}



#' @describeIn calc_iba
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param alpha Weight for TPR - TNR. By default 1.
#'
#' @export
#'
calc_iba.data.frame <- function(
    data,
    prediction, reference,
    alpha = 1
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_iba(tbl, alpha)

}
