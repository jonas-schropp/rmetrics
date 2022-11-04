
#' Calculate Diagnostic odds ratio
#'
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#'
#' @export
#'
calc_dor <- function(tp, fn, tn, fp) {

  (tp / (tp + fn)) / (fn / (fn + tn)) / (1 - (tp / (tp + fn))) / (tn / (fp + tn))

}
