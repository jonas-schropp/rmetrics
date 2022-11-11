#' Calculate False Positive Rate
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_fpr <- function(...) UseMethod("calc_fpr")



#' @describeIn calc_fpr
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fpr.default <- function(fp, tn, ci.type, ci.level, ...) {

  calc_prop(fp, fp + tn, ci.type, ci.level)

}



#' @describeIn calc_fpr
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fpr.table <- function(tbl, ci.type, ci.level, ...) {

  tn <- tbl[1,1]
  fp <- tbl[2,1]

  calc_prop(fp, fp + tn, ci.type, ci.level)

}



#' @describeIn calc_fpr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fpr.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_fpr(tbl, ci.type, ci.level)

}
