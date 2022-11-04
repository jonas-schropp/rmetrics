
#' Calculate False Positive Rate
#'
#' @param fp Number of false positives in the contingency table.
#' @param tn Number of true negatives in the contingency table.
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level A number between 0 and 1 for the levels of the confidence intervals that should be calculated.
#'
#' @export
#'
calc_fpr <- function(fp, tn, ci.type, ci.level) {

  calc_prop(fp, fp + tn, ci.type, ci.level)

}
