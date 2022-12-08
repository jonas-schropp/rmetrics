#' Calculate False Negative Rate
#'
#' The false negative rate is a measure of the proportion of false negative
#' results in a binary classification problem. It is calculated as the number
#' of false negative predictions divided by the total number of actual
#' positive samples.
#'
#' @details
#' To calculate the false negative rate, the following formula is used:
#'
#' False negative rate = (Number of false negative predictions) / (Total number of actual positive samples)
#'
#' The resulting value is a proportion, with values closer to 0 indicating a
#' better-performing model and values closer to 1 indicating a worse-
#' performing model.
#'
#' Overall, the false negative rate is a useful metric for evaluating
#' the performance of a classification model. It provides a way to compare
#' the ability of different models to accurately predict the positive class,
#' and can be used to identify models that have a high degree of accuracy for
#' the positive class.
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
#' @param ci.type `r rox("prop.ci.type")`
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
#' @param ci.type `r rox("prop.ci.type")`
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
#' @param ci.type `r rox("prop.ci.type")`
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
