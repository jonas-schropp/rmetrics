#' Calculate Information score.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_is <- function(...) UseMethod("calc_is")



#' @describeIn calc_is
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_is.default <- function(tp, fp, fn, n, ...) {

  if ((tp + fp) == 0) {

    warning("Can not calculate information score if both TP and FP are 0.
            Returning NA.")

    return(NA_real_)

  } else {

    -log(((tp + fn) / n), base = 2) + log((tp / (tp + fp)), base = 2)

  }

}



#' @describeIn calc_is
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_is.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]
  n <- sum(tbl)

  calc_is.default(tp, fp, fn, n)

}



#' @describeIn calc_is
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_is.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_is.table(tbl, incr)

}

