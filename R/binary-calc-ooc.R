#' Calculate Otsuka-Ochiai Coefficient
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_ooc <- function(...) UseMethod("calc_ooc")



#' @describeIn calc_ooc
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_ooc.default <- function(tp, fp, fn, ...) {

  ppos <- tp + fp
  pos <- tp + fn

  if (ppos > 0 & pos > 0) {

    ooc <- tp / sqrt(ppos * pos)

  } else {

    ooc <- NA_real_

    warning(
      "Can not calculate Otsuka-Ochiai coefficient. \n
      No positive cases in prediction or reference.")

  }

  ooc

}



#' @describeIn calc_ooc
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_ooc.table <- function(tbl, ...) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_ooc(tp, fp, fn)

}



#' @describeIn calc_ooc
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_ooc.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_ooc(tbl)

}
