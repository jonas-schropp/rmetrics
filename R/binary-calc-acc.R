#' Calculate Accuracy (acc)
#'
#'
#' Accuracy refers to the degree to which a model's predictions are correct.
#' It is a measure of how well a model is able to make predictions that match
#' the observed data.
#' Accuracy is calculated by dividing the number of correct predictions made
#' by the model by the total number of predictions made.
#'
#' @details
#' Accuracy is calculated as follows:
#'
#' Accuracy = (Number of correct predictions) / (Total number of predictions)
#'
#' For example, if a model makes 100 predictions and 75 of them are correct,
#' the accuracy score would be 75/100 = 0.75. This means that the model is able
#' to correctly predict the outcome 75% of the time.
#'
#' Use with caution when class imbalance is present or a focus on either type
#' 1 or type 2 errors is necessary.
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
#' @param ci.type `r rox("prop.ci.type")`
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
#' @param ci.type `r rox("prop.ci.type")`
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
#' @param ci.type `r rox("prop.ci.type")`
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
