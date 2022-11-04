
#' Calculate Area under the ROC curve for each class (auroc).
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_auroc <- function(tn, fp, tp, fn) {

  tnr <- (calc_tnr(
    tn = tn, fp = fp, ci.type = F, ci.level = 0)
    )[1]

  tpr <- (calc_tpr(
    tp = tp, fn = fn, ci.type = F, ci.level = 0)
    )[1]

  (tnr + tpr) / 2

}
