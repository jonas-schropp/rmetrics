#' Calculate Matthews Correlation Coefficient
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_mcc <- function(...) UseMethod("calc_mcc")



#' @describeIn calc_mcc
#'
#' @param tp `r rox("tp")`
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_mcc.default <- function(tp, tn, fp, fn, ...) {

  denom <- (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)

  if (denom != 0) {

    mcc <- (tp*tn - fp*fn) / sqrt(denom)

  } else {

    mcc <- NA_real_

    warning("Can not calculate Matthews correlation coefficient.
            Table contains two zero cells. Returning NA.")

  }

  mcc

}



#' @describeIn calc_mcc
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_mcc.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_mcc.default(tp, tn, fp, fn)

}



#' @describeIn calc_mcc
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_mcc.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_mcc.table(tbl, incr)

}
