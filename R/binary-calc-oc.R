#' Calculate Overlap Coefficient
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_oc <- function(...) UseMethod("calc_oc")



#' @describeIn calc_oc
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_oc.default <- function(tp, fp, fn, ...) {

  ppos <- tp + fp
  pos <- tp + fn
  m <- min(ppos, pos)


  if (m != 0) {

    oc <- tp / m

  } else {

    oc <- NA_real_

    warning(
      "Can not calculate Overlap coefficient. \n
      No positive cases in prediction or reference. \n
      Returning NA.")

  }

  oc

}



#' @describeIn calc_oc
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_oc.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_oc.default(tp, fp, fn)

}



#' @describeIn calc_oc
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_oc.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_oc.table(tbl, incr)

}

