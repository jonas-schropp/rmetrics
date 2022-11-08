#' Calculate Area under the PR curve (AUPR).
#'
#' @export
#'
calc_aupr <- function(...) UseMethod("calc_aupr")



#' @describeIn calc_aupr
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_aupr.default <- function(tp, fp, fn) {

  ppv <- calc_precision(tp, fp, FALSE, 0)[1]
  tpr <- calc_tpr(tp, fn, FALSE, 0)[1]

  (ppv + tpr) / 2

}



#' @describeIn calc_aupr
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_aupr.table <- function(tbl) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_aupr(tp, fp, fn)

}



#' @describeIn calc_aupr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_aupr.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_aupr(tbl)

}
