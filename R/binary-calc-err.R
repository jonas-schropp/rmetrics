#' Calculate Error Rate.
#'
#' The error rate of a classification model is a measure of the proportion
#' of incorrect predictions made by the model. It is calculated as the number
#' of incorrect predictions divided by the total number of predictions made by
#' the model.
#'
#' @details
#' To calculate the error rate of a classification model, the following formula is used:
#'
#' Error rate = (Number of incorrect predictions) / (Total number of predictions)
#'
#' The resulting value is a proportion, with values closer to 0 indicating a
#' better-performing model and values closer to 1 indicating a worse-
#' performing model.
#'
#' @param ... `r rox("dots")`
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
#' @param ci.type `r rox("prop.ci.type")`
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_err.default <- function(
    tp, tn, fp, fn,
    ci.type, ci.level,
    ...
    ) {

  calc_prop(fp+fn,
            tp+tn+fp+fn,
            ci.type = ci.type,
            ci.level = ci.level)

}



#' @describeIn calc_err
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type `r rox("prop.ci.type")`
#' @param ci.level `r rox("ci.level")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_err.table <- function(
    tbl,
    ci.type,
    ci.level,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_prop(fp + fn,
            sum(tbl),
            ci.type = ci.type,
            ci.level = ci.level)

}



#' @describeIn calc_err
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type `r rox("prop.ci.type")`
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_err.data.frame <- function(
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

  calc_err.table(tbl, ci.type, ci.level, incr)

}
