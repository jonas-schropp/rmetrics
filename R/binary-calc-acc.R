#' Calculate Accuracy (acc)
#'
#' @export
#'
calc_acc <- function(...) UseMethod("calc_acc")



#' @describeIn calc_acc
#'
#' @param tp `r rox("tp")`
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_acc.default <- function(
    tp, tn, fp, fn,
    ci.type, ci.level,
    ...
    ) {

  calc_prop(tp + tn,
            tp + tn + fp + fn,
            ci.type,
            ci.level)

}



#' @describeIn calc_acc
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_acc.table <- function(tbl,
                           ci.type, ci.level,
                           ...) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_prop(tp + tn, tp + tn + fp + fn, ci.type, ci.level)

}



#' @describeIn calc_acc
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_acc.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_acc(tbl, ci.type, ci.level)

}
