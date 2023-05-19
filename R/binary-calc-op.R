#' Calculate Optimized Precision.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_op <- function(...) UseMethod("calc_op")



#' @describeIn calc_op
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_op.default <- function(tn, fp, tp, fn, ...) {

  acc <- calc_acc(tp, tn, fp, fn, F, 0)[1]
  tnr <- calc_tnr(tn, fp, F, 0)[1]
  tpr <- calc_tpr(tp, fn, F, 0)[1]

  if (tpr == 0 & tnr == 0) {
    warning("Can't calculate optimized precision if both TPR and TNR are 0.
            Returning NA.")
    return(NA_real_)
  }

  ri <- abs(tnr - tpr) / (tpr + tnr)

  return(acc - ri)

}



#' @describeIn calc_op
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_op.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_op.default(tn, fp, tp, fn)

}



#' @describeIn calc_op
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_op.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_op.table(tbl, incr)

}
