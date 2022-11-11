#' Calculate Geometric mean (gmean) of TPR and TNR.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_gmean <- function(...) UseMethod("calc_gmean")



#' @describeIn calc_gmean
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_gmean.default <- function(tn, fp, tp, fn, ...) {

  tnr <- calc_tnr(tn, fp, FALSE, 0)[1]
  tpr <- calc_tpr(tp, fn, FALSE, 0)[1]

  calc_g(tpr, tnr)

}



#' @describeIn calc_gmean
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_gmean.table <- function(tbl, ...) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_gmean(tn, fp, tp, fn)

}



#' @describeIn calc_gmean
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_gmean.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_gmean(tbl)

}
