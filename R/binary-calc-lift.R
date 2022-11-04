
#' Calculate Lift score (lift).
#'
#' @param tp Number of true positives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param pos Number of actual positives
#' @param neg Number of actual negatives
#'
#' @export
#'
calc_lift <- function(tp, fp, pos, neg) {

  precision <- (calc_precision(tp, fp, ci.type = FALSE, ci.level = 0))[1]
  prevalence <- (calc_prevalence(pos = pos, neg = neg, ci.type = FALSE, ci.level = 0))[1]

  precision / prevalence

}
