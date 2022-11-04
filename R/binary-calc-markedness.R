
#' Calculate Markedness
#'
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#'
#' @export
#'
calc_markedness <- function(tp, fn, tn, fp) {

  (tp / (tp + fp)) + (tn / (tn + fn)) - 1

}
