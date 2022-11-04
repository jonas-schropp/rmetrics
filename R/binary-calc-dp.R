
#' Calculate Discriminant power (dp).
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_dp <- function(tn, fp, tp, fn) {

  tnr <- (calc_tnr(
    tn = tn, fp = fp, ci.type = FALSE, ci.level = 0)
  )[1]

  tpr <- (calc_tpr(
    tp = tp, fn = fn, ci.type = FALSE, ci.level = 0)
  )[1]

  if (tpr == 1 | tnr == 1) {
    warning("Discriminant power can not be calculated with
             perfect tpr or tnr.")
  }

  (sqrt(3) / pi) * log((tpr / (1 - tpr)), 10) + log((tnr / (1 - tnr)), 10)

}
