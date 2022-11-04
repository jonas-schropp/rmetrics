
#' Calculate Information score (is).
#'
#' @param tp Number of true positives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#' @param n Total number of samples.
#'
#' @export
#'
calc_is <- function(tp, fp, fn, n) {
  -log(((tp + fn) / n), base = 2) + log((tp / (tp + fp)), base = 2)
}
