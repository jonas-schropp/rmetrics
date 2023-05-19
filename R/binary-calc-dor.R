#' Calculate Diagnostic odds ratio
#'
#' The diagnostic odds ratio (DOR) is a metric that is used to evaluate
#' the performance of a diagnostic test. It is calculated as the ratio of
#' the odds of a positive test result in people with the disease to the
#' odds of a positive test result in people without the disease.
#'
#' @details
#' To calculate the DOR, the following formula is used:
#'
#' DOR = (True positive rate / False positive rate) / (False negative rate /
#' True negative rate)
#'
#' where the true positive rate is the ratio of true positive predictions to
#' the total number of actual positive samples, the false positive rate is
#' the ratio of false positive predictions to the total number of actual
#' negative samples, the false negative rate is the ratio of false
#' negative predictions to the total number of actual positive samples,
#' and the true negative rate is the ratio of true negative predictions to
#' the total number of actual negative samples.
#'
#' The resulting value is a measure of the effectiveness of a diagnostic test,
#' with values closer to 1 indicating a weaker test and values closer to
#' infinity indicating a stronger test.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_dor <- function(...) UseMethod("calc_dor")



#' @describeIn calc_dor
#'
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#'
#' @export
#'
calc_dor.default <- function(tp, fn, tn, fp, ...) {

  plr <- calc_plr(tp, fn, fp, tn, F, 0)[1]
  nlr <- calc_nlr(tp, fn, fp, tn, F, 0)[1]

  if (is.na(nlr) | nlr == 0) {
    warning("Can not calculate DOR if NLR is 0 or NA. Returning NA.")
    return(NA_real_)
  } else {
    return(plr / nlr)
  }

}



#' @describeIn calc_dor
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_dor.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_dor.default(
    tp,
    fn,
    tn,
    fp
    )

}



#' @describeIn calc_dor
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_dor.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_dor.table(tbl, incr)

}
