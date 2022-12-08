#' Calculate (adjusted) F-Score
#'
#' The F-score, also known as the F1-score or F-measure, is a metric that
#' combines precision and recall into a single score. This implementation
#' allows you to set the parameter `beta` to control the relative weight of precision and recall.
#'
#' @details
#' To calculate the adjusted F-score, the following formula is used:
#'
#' Adjusted F-score = (1 + beta^2) \* (Precision \* Recall) / (beta^2 \* Precision + Recall)
#'
#' where beta is a parameter that controls the relative weight of precision
#' and recall. For beta = 1, the adjusted F-score is equivalent to the F1-score.
#' For beta < 1, precision is given more weight, and for beta > 1, recall is
#' given more weight.
#'
#' Overall, the adjusted F-score is a useful metric for evaluating the
#' performance of a predictive model, particularly when the precision and
#' recall are calculated using a sample of the data. It allows for a more
#' balanced evaluation of a model's performance by taking into account the
#' number of data points used to calculate the precision and recall.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_f <- function(...) UseMethod("calc_f")



#' @describeIn calc_f
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#' @param beta beta coefficient
#'
#' @export
#'
calc_f.default <- function(tp, fp, fn, beta, ...) {

  ((1 + (beta)^2) * tp) / ((1 + (beta)^2) * tp + fp + (beta^2) * fn)

 }



#' @describeIn calc_f
#'
#' @param tbl `r rox("tbl")`
#' @param beta beta coefficient
#'
#' @export
#'
calc_f.table <- function(tbl, beta, ...) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_f(tp, fp, fn, beta)

}



#' @describeIn calc_f
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param beta beta coefficient
#'
#' @export
#'
calc_f.data.frame <- function(
    data,
    prediction, reference,
    beta, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_f(tbl, beta)

}
