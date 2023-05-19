#' Calculate Phi Coefficient
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_phi <- function(...) UseMethod("calc_phi")



#' @describeIn calc_phi
#'
#' @param tp `r rox("tp")`
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_phi.default <- function(
    tp, tn, fp, fn,
    ...) {

  denom <- (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)

  if (denom == 0) {
    warning("calc_phi can not be calculated if any combination of two cells
            is zero. Returning NA.")
    return(NA_real_)
  }

  (tp*tn - fp*fn) / sqrt(denom)

}



#' @describeIn calc_phi
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_phi.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_phi.default(tp, tn, fp, fn)

}



#' @describeIn calc_phi
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_phi.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
    ) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_phi.table(tbl, incr)

}
