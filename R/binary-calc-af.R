#' Calculate Adjusted F-score (af).
#'
#' @export
#'
calc_af <- function(...) {

  UseMethod("calc_af")

}



#' @describeIn calc_af
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#' @param tn `r rox("tn")`
#'
#' @export
#'
calc_af.default <- function(tp, fp, fn, tn) {

  f2 <- calc_f(tp = tp, fp = fp, fn = fn, beta = 2)
  inv05 <- calc_f(tp = tn, fp = fn, fn = fp, beta = 0.5)

  sqrt(f2 * inv05)

}



#' @describeIn calc_af
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_af.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_af(tp, fp, fn, tn)

}



#' @describeIn calc_af
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_af.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_af(tbl)

}
