#' Calculate Size of Grey Area
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_grey <- function(...) UseMethod("calc_grey")



#' @describeIn calc_grey
#'
#' @param ind Number of indeterminate results.
#' @param n `r rox("n")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_grey.default <- function(ind, n, ci.type, ci.level, ...) {

  calc_prop(ind, n, ci.type, ci.level)

}



#' @describeIn calc_grey
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_grey.table <- function(tbl, ci.type, ci.level, ...) {

  n <- sum(tbl)
  ind <- sum(tbl[,2])

  calc_prop(ind, n, ci.type, ci.level)

}



#' @describeIn calc_grey
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_grey.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_grey(tbl, ci.type, ci.level)

}
