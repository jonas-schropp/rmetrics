#' Calculate False Omission Rate
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_for <- function(...) UseMethod("calc_for")



#' @describeIn calc_for
#'
#' @param fn `r rox("fn")`
#' @param tn `r rox("tn")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_for.default <- function(fn, tn, ci.type, ci.level, ...) {

   calc_prop(fn, fn + tn, ci.type, ci.level)

}



#' @describeIn calc_for
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
 calc_for.table <- function(tbl, ci.type, ci.level, ...) {

  tn <- tbl[1,1]
  fn <- tbl[1,2]

  calc_prop(fn, fn + tn, ci.type, ci.level)

}



#' @describeIn calc_for
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
 calc_for.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_for(tbl, ci.type, ci.level)

}
