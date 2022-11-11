#' Calculate Sample Prevalence
#'
#' @export
#'
calc_prevalence <- function(...) UseMethod("calc_prevalence")



#' @describeIn calc_prevalence
#'
#' @param pos `r rox("pos")`
#' @param neg `r rox("neg")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_prevalence.default <- function(pos, neg, ci.type, ci.level) {

  calc_prop(pos, pos + neg, ci.type, ci.level)

}



#' @describeIn calc_prevalence
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_prevalence.table <- function(tbl, ci.type, ci.level) {

  pos <- sum(tbl[,2])
  neg <- sum(tbl[,1])

  calc_prop(pos, pos + neg, ci.type, ci.level)

}



#' @describeIn calc_prevalence
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_prevalence.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_prevalence(tbl, ci.type, ci.level)

}
