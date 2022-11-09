#' Calculate Phi Coefficient
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
calc_phi.default <- function(tp, tn, fp, fn) {

  (tp*tn - fp*fn) / sqrt( (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn) )

}



#' @describeIn calc_phi
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_phi.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_phi(tp, tn, fp, fn)

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
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_phi(tbl)

}
