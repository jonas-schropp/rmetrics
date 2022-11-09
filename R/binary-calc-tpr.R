#' Calculate Sensitivity (TPR)
#'
#' @export
#'
calc_tpr <- function(...) UseMethod("calc_tpr")



#' @describeIn calc_tpr
#'
#' @param tp `r rox("tp")`
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_tpr.default <- function(tp, fn, ci.type, ci.level) {

  calc_prop(tp, tp + fn, ci.type, ci.level)

}



#' @describeIn calc_tpr
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_tpr.table <- function(tbl, ci.type, ci.level) {

  tp <- tbl[2,2]
  fn <- tbl[1,2]

  calc_prop(tp, tp + fn, ci.type, ci.level)

}



#' @describeIn calc_tpr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_tpr.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_tpr(tbl, ci.type, ci.level)

}
