
#' Calculate Gini index (gini).
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_gini <- function(tn, fp, tp, fn) {

  auroc <- (calc_auroc(tn = tn, fp = fp, tp = tp, fn = fn))[1]

  2 * auroc - 1

}
