#' Calculate Fowlkes–Mallows Index.
#'
#' @param ... `r rox("dots")`
#'
#' The Fowlkes–Mallows index is the geometric mean of precision (PPV) and recall (TPR). It is generally used to compare the results of two clustering algorithms. It ranges from 0 to 1, with 1 indicating perfect classification.
#'
#' @references
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983). "A Method for Comparing Two Hierarchical Clusterings". Journal of the American Statistical Association. 78 (383): 553. doi:10.2307/2288117
#'
#'
#' @export
#'
calc_fmi <- function(...) UseMethod("calc_fmi")



#' @describeIn calc_fmi
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_fmi.table <- function(tbl, ...) {

  n <- sum(tbl)
  tk <- sum(tbl^2) - n
  ppos <- colSums(tbl)
  pos <- rowSums(tbl)

  pk <- sum(ppos^2) - n
  qk <- sum(pos^2) - n

  return(tk / sqrt(pk * qk))

}



#' @describeIn calc_fmi
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_fmi.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_fmi(tbl)

}
