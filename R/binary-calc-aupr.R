
#' Calculate Area under the PR curve for each class (aupr).
#'
#' @param tp Number of true positives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_aupr <- function(tp, fp, fn) {

  ppv <- (calc_precision (
    tp = tp, fp = fp, ci.type = FALSE, ci.level = 0)
  )[1]

  tpr <- (calc_tpr(
    tp = tp, fn = fn, ci.type = FALSE, ci.level = 0)
  )[1]

  (ppv + tpr) / 2

}
