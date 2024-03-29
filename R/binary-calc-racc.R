#' Calculate (Unbiased) Random Accuracy
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_racc <- function(...) UseMethod("calc_racc")



#' @describeIn calc_racc
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#' @param n `r rox("n")`
#' @param unbiased Should unbiased random accuracy be calculated? FALSE by default.
#'
#' @export
#'
calc_racc.default <- function(tp, fp, fn, n, unbiased = FALSE, ...) {

  ppos <- tp + fp
  pos <- tp + fn

  if(unbiased) {
    ((ppos + pos) / (2 * n))^2
  } else {
    (ppos * pos) / (n^2)
  }

}



#' @describeIn calc_racc
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_racc.table <- function(
    tbl,
    unbiased = FALSE,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]
  n <- sum(tbl)

  calc_racc.default(tp, fp, fn, n, unbiased)

}



#' @describeIn calc_racc
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_racc.data.frame <- function(
    data,
    prediction,
    reference,
    unbiased = FALSE,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_racc.table(tbl, unbiased, incr)

}

