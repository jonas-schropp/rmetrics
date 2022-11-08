#' Calculate Bray-Curtis dissimilarity (bcd).
#'
#' @export
#'
calc_bcd <- function(...) UseMethod("calc_bcd")



#' @describeIn calc_bcd
#'
#' @param ppos `r rox("ppos")`
#' @param pos `r rox("pos")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_bcd.default <- function(ppos, pos, n) {

  am <- calc_am(ppos, pos)

  abs(am) / (2 * n)

}



#' @describeIn calc_bcd
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_bcd.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_bcd(ppos, pos, n)

}



#' @describeIn calc_bcd
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_bcd.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_bcd(tbl)

}
