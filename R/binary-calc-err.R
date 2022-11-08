#' Calculate Error rate (err).
#'
#' @export
#'
 calc_err <- function(...) UseMethod("calc_err")



#' @describeIn calc_err
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
calc_err.default <- function(tp, tn, fp, fn, ci.type, ci.level) {

  calc_prop(fp+fn,
            tp+tn+fp+fn,
            ci.type = ci.type,
            ci.level = ci.level)

}



#' @describeIn calc_err
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_err.table <- function(tbl, ci.type, ci.level) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_prop(fp+fn,
            tp+tn+fp+fn,
            ci.type = ci.type,
            ci.level = ci.level)

}



#' @describeIn calc_err
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_err.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_err(tbl)

}
