
#' Calculate Discriminant Power (dp).
#'
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_dp <- function(tn, fp, tp, fn) {

  tpr <- calc_tpr(tp, fn, F, 0)[1]
  tnr <- calc_tnr(tn, fp, F, 0)[1]

  if (tpr == 1 | tnr == 1) {
    warning("Discriminant power can not be calculated with
             perfect tpr or tnr. Returning NA.")
    return(NA_real_)
  }

  a <- tpr / (1 - tpr)
  b <- tnr / (1 - tnr)

  (sqrt(3) / pi) * (log(a, 10) + log(b, 10))

}
