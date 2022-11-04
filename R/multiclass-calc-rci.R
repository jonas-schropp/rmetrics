
#' Calculate Relative classifier information (rci).
#'
#' @param tbl confusion matrix
#' @param pos vector of actual positives per class
#' @param ppos vector of positives in predict vector per class
#' @param n Total number of observations
#'
#' @export
#'
calc_rci <- function(tbl, pos, ppos, n) {

  mutual_information <- calc_mutual_information(tbl, pos, ppos, n)
  reference_entropy <- calc_reference_entropy(pos, n)

  if (reference_entropy != 0) {
    mutual_information / reference_entropy
  } else {
    NA
  }

}
