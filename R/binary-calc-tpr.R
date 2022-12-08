#' Calculate Sensitivity (TPR)
#'
#' Sensitivity is the ability of a test or measurement to accurately detect what
#' it is supposed to detect. In the context of medical tests, sensitivity refers
#' to the proportion of people with a disease who are correctly identified as
#' having the disease by the test. A test with high sensitivity will have few
#' false negatives, meaning that it will rarely miss cases of the disease.
#'
#' For example, a test for a certain type of cancer may have a sensitivity of 95%,
#' which means that out of 100 people with the cancer, the test will correctly
#' identify 95 of them as having the disease. This means that the test will have
#' a 5% false negative rate, which means that out of 100 people with the cancer,
#' the test will fail to detect the disease in 5 of them.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_tpr <- function(...) UseMethod("calc_tpr")



#' @describeIn calc_tpr
#'
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_tpr.default <- function(tp, fn, ci.type, ci.level, ...) {

  calc_prop(tp, tp + fn, ci.type, ci.level)

}



#' @describeIn calc_tpr
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_tpr.table <- function(tbl, ci.type, ci.level, ...) {

  tp <- tbl[2,2]
  fn <- tbl[1,2]

  calc_prop(tp, tp + fn, ci.type, ci.level)

}



#' @describeIn calc_tpr
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
calc_tpr.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_tpr(tbl, ci.type, ci.level)

}
