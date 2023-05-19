#' Calculate Specificity (TNR)
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_tnr <- function(...) UseMethod("calc_tnr")



#' @describeIn calc_tnr
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_tnr.default <- function(tn, fp, ci.type, ci.level, ...) {

  calc_prop(tn, tn + fp, ci.type, ci.level)

}



#' @describeIn calc_tnr
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of
#' "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald",
#' "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If
#' FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_tnr.table <- function(
    tbl,
    ci.type,
    ci.level,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tn <- tbl[1,1]
  fp <- tbl[2,1]

  calc_prop(tn, tn + fp, ci.type, ci.level)

}



#' @describeIn calc_tnr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_tnr.data.frame <- function(
    data,
    prediction,
    reference,
    ci.type,
    ci.level,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_tnr.table(tbl, ci.type, ci.level, incr)

}
