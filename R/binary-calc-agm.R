#' Calculate Adjusted geometric mean (agm).
#'
#' @export
#'
calc_agm <- function(...) UseMethod("calc_agm")



#' @describeIn calc_agm
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_agm.default <- function(tn, fp, tp, fn) {

  tnr <- calc_tnr(tn, fp, FALSE, 0)[1]
  tpr <- calc_tpr(tp, fn, FALSE, 0)[1]
  gm <- calc_g(tnr, tpr)

  neg <- tn + fp
  n <- neg + tp + fn
  neg <- neg / n

  if (tpr == 0) {

    agm <- 0

  } else if (neg == 0) {

    agm <- NA_real_
    warning("Can not calculate adjusted geometric mean. \n
            No negatives in reference.")

  } else {

    agm <- (gm + tnr * neg) / (1 + neg)

  }

  agm

}



#' @describeIn calc_agm
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_agm.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_agm(tn, fp, tp, fn)

}



#' @describeIn calc_agm
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_agm.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_agm(tbl)

}
