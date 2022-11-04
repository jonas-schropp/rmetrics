
#' Calculate Optimized precision (op).
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_op <- function(tn, fp, tp, fn) {

  acc <- (calc_acc(tp = tp, tn = tn, fp = fp, fn = fn,
                   ci.type = FALSE, ci.level = 0))[1]

  tnr <- (calc_tnr(tn = tn, fp = fp,
                   ci.type = FALSE, ci.level = 0))[1]

  tpr <- (calc_tpr(tp = tp, fn = fn,
                   ci.type = FALSE, ci.level = 0))[1]

  acc - (abs(tnr - tpr) / (tpr + tnr))

}
