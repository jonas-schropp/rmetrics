
#' Calculate Sample Prevalence
#'
#' @param pos Number of positives according to reference.
#' @param neg Number of negatives according to reference.
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level A number between 0 and 1 for the levels of the confidence intervals that should be calculated.
#'
#' @export
#'
calc_prevalence <- function(pos, neg, ci.type, ci.level) {

  calc_prop(pos, pos + neg, ci.type, ci.level)

}
