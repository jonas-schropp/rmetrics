#' Calculate Precision
#'
#' @export
#'
calc_precision <- function(...) UseMethod("calc_precision")



#' @describeIn calc_precision
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_precision.default <- function(tp, fp, ci.type, ci.level) {

  calc_prop(tp, tp + fp, ci.type, ci.level)

}



#' @describeIn calc_precision
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_precision.table <- function(tbl, ci.type, ci.level) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]

  calc_prop(tp, tp + fp, ci.type, ci.level)

}



#' @describeIn calc_precision
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_precision.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_precision(tbl, ci.type, ci.level)

}
