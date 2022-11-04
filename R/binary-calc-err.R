
#' Calculate Error rate (err).
#'
#' @param tp Number of true positives in the contingency table.
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level A number between 0 and 1 for the levels of the confidence intervals that should be calculated.
#'
#' @export
#'
calc_err <- function(tp, tn, fp, fn, ci.type, ci.level) {

  calc_prop(fp+fn,
            tp+tn+fp+fn,
            ci.type = ci.type,
            ci.level = ci.level)

}
