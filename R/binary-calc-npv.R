
#' Calculate Negative Predictive Value
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level A number between 0 and 1 for the levels of the confidence intervals that should be calculated.
#'
#' @export
#'
calc_npv <- function(tn, fn, ci.type, ci.level) {

  calc_prop(tn, tn + fn, ci.type, ci.level)

}
