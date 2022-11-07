
#' Calculate Optimized precision (op).
#'
#'
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_op <- function(tn, fp, tp, fn) {

  acc <- calc_acc(tp, tn, fp, fn, F, 0)[1]
  tnr <- calc_tnr(tn, fp, F, 0)[1]
  tpr <- calc_tpr(tp, fn, F, 0)[1]

  if (tpr == 0 & tnr == 0) {
    warning("Can't calculate optimized precision if both TPR and TNR are 0.
            Returning NA.")
    return(NA_real_)
  }

  ri <- abs(tnr - tpr) / (tpr + tnr)

  return(acc - ri)

}
