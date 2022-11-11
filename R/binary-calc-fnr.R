#' Calculate False Negative Rate
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
 calc_fnr <- function(...) UseMethod("calc_fnr")



#' @describeIn calc_fnr
#'
#' @param fn `r rox("fn")`
#' @param tp `r rox("tp")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
 calc_fnr.default <- function(fn, tp, ci.type, ci.level, ...) {

   calc_prop(fn, fn + tp, ci.type, ci.level)

 }



#' @describeIn calc_fnr
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fnr.table <- function(tbl, ci.type, ci.level, ...) {

  tp <- tbl[2,2]
  fn <- tbl[1,2]

  calc_prop(fn, fn + tp, ci.type, ci.level)

}



#' @describeIn calc_fnr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fnr.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_fnr(tbl, ci.type, ci.level)

}
