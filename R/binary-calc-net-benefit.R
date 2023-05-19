#' Calculate Net Benefit.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_net_benefit <- function(...) UseMethod("calc_net_benefit")



#' @describeIn calc_net_benefit
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param n `r rox("n")`
#' @param weight The weight for FP in comparison to TP. By default 1.
#'
#' @export
#'
calc_net_benefit.default <- function(tp, fp, n, weight = 1, ...) {

  (tp - weight * fp) / n

}



#' @describeIn calc_net_benefit
#'
#' @param tbl `r rox("tbl")`
#' @param weight The weight for FP in comparison to TP. By default 1.
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_net_benefit.table <- function(
    tbl,
    weight = 1,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  n <- sum(tbl)

  calc_net_benefit.default(tp, fp, n, weight)

}



#' @describeIn calc_net_benefit
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param weight The weight for FP in comparison to TP. By default 1.
#'
#' @export
#'
calc_net_benefit.data.frame <- function(
    data,
    prediction,
    reference,
    weight = 1,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_net_benefit.table(tbl, weight, incr)

}
