#' Calculate False Discovery Rate
#'
#' @export
#'
calc_fdr <- function(...) UseMethod("calc_fdr")



#' @describeIn calc_fdr
#'
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fdr.default <- function(fp, tp, ci.type, ci.level) {

  calc_prop(fp, tp, ci.type, ci.level)

}



#' @describeIn calc_fdr
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fdr.table <- function(tbl, ci.type, ci.level) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]

  calc_prop(fp, tp, ci.type, ci.level)

}



#' @describeIn calc_fdr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fdr.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_fdr(tbl, ci.type, ci.level)

}
