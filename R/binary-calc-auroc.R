#' Calculate Area under the ROC curve for each class (auroc).
#'
#' @export
#'
calc_auroc <- function(...) UseMethod("calc_auroc")



#' @describeIn calc_auroc
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_auroc.default <- function(tn, fp, tp, fn) {

  tnr <- calc_tnr(tn, fp, F, 0)[1]
  tpr <- calc_tpr(tp, fn, F, 0)[1]

  (tnr + tpr) / 2

}



#' @describeIn calc_auroc
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_auroc.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_auroc(tn, fp, tp, fn)

}



#' @describeIn calc_auroc
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_auroc.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_auroc(tbl)

}

