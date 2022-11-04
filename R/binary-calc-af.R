
#' Calculate Adjusted F-score (af).
#'
#' @param tp Number of true positives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#' @param tn Number of true negatives in the contingency table.
#'
#' @export
#'
calc_af <- function(tp, fp, fn, tn) {

  f2 = calc_f(tp = tp, fp = fp, fn = fn, beta = 2)
  inv05 = calc_f(tp = tn, fp = fn, fn = fp, beta = 0.5)

  sqrt(f2 * inv05)

}
