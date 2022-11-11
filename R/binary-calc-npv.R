#' Calculate Negative Predictive Value
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_npv <- function(...) UseMethod("calc_npv")



#' @describeIn calc_npv
#'
#' @param tn `r rox("tn")`
#' @param fn `r rox("fn")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_npv.default <- function(tn, fn, ci.type, ci.level, ...) {

  calc_prop(tn, tn + fn, ci.type, ci.level)

}



#' @describeIn calc_npv
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_npv.table <- function(tbl, ci.type, ci.level, ...) {

  tn <- tbl[1,1]
  fn <- tbl[1,2]

  calc_prop(tn, tn + fn, ci.type, ci.level)

}



#' @describeIn calc_npv
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_npv.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_npv(tbl, ci.type, ci.level)

}
