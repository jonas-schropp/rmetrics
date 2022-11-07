
#' Calculate Random acc Unbiased
#'
#' @param tp Number of true positives
#' @param fp Number of false positives
#' @param fn Number of false negatives
#' @param n Total number of samples
#'
#' @export
#'
calc_raccu <- function(tp, fp, fn, n) {

  ppos <- tp + fp
  pos <- tp + fn

  ((ppos + pos) / (2 * n))^2

}
