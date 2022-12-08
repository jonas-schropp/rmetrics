#' Calculate False Positive Rate
#'
#' The false positive rate (FPR) is a measure of the proportion of false
#' positive results in a binary classification problem. It is calculated as
#' the number of false positive predictions divided by the total number of
#' actual negative samples.
#'
#' @details
#' To calculate the FPR, the following formula is used:
#'
#' False positive rate = (Number of false positive predictions) / (Total number of actual negative samples)
#'
#' The resulting value is a proportion, with values closer to 0 indicating a
#' better-performing model and values closer to 1 indicating a worse-
#' performing model.
#'
#' The FPR is a useful metric to compare the ability of different models
#' to accurately predict the negative class, and can be used to identify
#' models that have a high degree of accuracy for the negative class.
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
#' @param ci.type `r rox("prop.ci.type")`
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
#' @param ci.type `r rox("prop.ci.type")`
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
#' @param ci.type `r rox("prop.ci.type")`
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
