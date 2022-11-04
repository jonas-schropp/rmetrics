
#' Calculate False Discovery Rate
#'
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level A number between 0 and 1 for the levels of the confidence intervals that should be calculated.
#'
#' @export
#'
calc_fdr <- function(fp, tp, ci.type, ci.level) {

  calc_prop(fp, fp + tp, ci.type, ci.level)

}
