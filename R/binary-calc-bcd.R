#' Calculate Bray-Curtis dissimilarity.
#'
#' The Bray-Curtis dissimilarity is a measure of the degree to which two sets
#' of observations are different. It is commonly used in ecology to compare
#' the composition of two different plant communities, but it can also be
#' applied to other types of data sets.
#'
#' @details
#' To calculate the Bray-Curtis dissimilarity, the following steps are
#' followed:
#'
#' - For each observation in the first set of data, count the number of times
#' it occurs.
#' - For each observation in the second set of data, count the number of times
#' it occurs.
#' - For each observation that occurs in both sets of data, calculate the
#' absolute difference between the two counts and add this value to the Bray-
#' Curtis dissimilarity score.
#' - Divide the Bray-Curtis dissimilarity score by the total number of
#' observations in both sets of data.
#'
#' The resulting value is a measure of the degree to which the two sets of data
#' are different, with values closer to 1 indicating greater dissimilarity
#' and values closer to 0 indicating less dissimilarity.
#'
#' @param ... `r rox("dots")`
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
calc_bcd.default <- function(ppos, pos, n, ...) {

  am <- calc_am(ppos, pos)

  abs(am) / (2 * n)

}



#' @describeIn calc_bcd
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_bcd.table <- function(tbl, ...) {

  ppos <- sum(tbl[2,])
  pos <- sum(tbl[,2])
  n <- sum(tbl)

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
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_bcd(tbl)

}
