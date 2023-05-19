#' Calculate Gini Index
#'
#'
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_gini <- function(...) UseMethod("calc_gini")



#' @describeIn calc_gini
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_gini.default <- function(tn, fp, tp, fn, ...) {

  auroc <- calc_auroc(tn, fp, tp, fn)[1]

  2 * auroc - 1

}



#' @describeIn calc_gini
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_gini.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_gini.default(tn, fp, tp, fn)

}



#' @describeIn calc_gini
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_gini.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_gini.table(tbl, incr)

}
