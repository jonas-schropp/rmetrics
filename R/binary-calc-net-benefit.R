#' Calculate Net Benefit.
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
calc_net_benefit.default <- function(tp, fp, n, weight = 1) {

  (tp - weight * fp) / n

}



#' @describeIn calc_net_benefit
#'
#' @param tbl `r rox("tbl")`
#' @param weight The weight for FP in comparison to TP. By default 1.
#'
#' @export
#'
calc_net_benefit.table <- function(tbl, weight = 1) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  n <- sum(tbl)

  calc_net_benefit(tp, fp, n, weight)

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
    prediction, reference,
    weight = 1
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_net_benefit(tbl, weight)

}
