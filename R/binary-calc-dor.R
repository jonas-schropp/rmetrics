
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

  plr <- calc_plr(tp, fn, fp, tn, F, 0)[1]
  nlr <- calc_nlr(fn, tp, tn, fp, F, 0)[1]

  plr / nlr

}
