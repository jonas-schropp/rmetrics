
#' Calculate Index of balanced acc (iba).
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#' @param alpha How much should TPR-TNR be weighted? Default is 1.
#'
#' @export
#'
calc_iba <- function(tn, fp, tp, fn, alpha = 1) {

  tnr <- (calc_tnr(tn = tn, fp = fp, ci.type = FALSE, ci.level = 0))[1]

  tpr <- (calc_tpr(tp = tp, fn = fn, ci.type = FALSE, ci.level = 0))[1]

  (1 + alpha * (tpr - tnr)) * tpr * tnr

}
