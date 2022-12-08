#' Calculate (adjusted) Geometric mean of TPR and TNR.
#'
#' The geometric mean is a measure of central tendency that is calculated by
#' taking the n-th root of the product of n numbers. In the context of binary
#' classification, the true positive rate (TPR) and true negative rate (TNR) are
#' two common metrics used to evaluate the performance of a model. The TPR is the
#' ratio of true positive predictions to the total number of positive samples,
#' and the TNR is the ratio of true negative predictions to the total number
#' of negative samples.
#'
#' @details
#' The geometric mean of TPR and TNR can be adjusted for the proportion of
#' negativesby setting `adjust = TRUE`.
#'
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_gmean <- function(...) UseMethod("calc_gmean")



#' @describeIn calc_gmean
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param adjust Should the GM be adjusted for the proportion of negatives?
#' FALSE by default.
#'
#' @export
#'
calc_gmean.default <- function(tn, fp, tp, fn, adjust = FALSE, ...) {

  tnr <- calc_tnr(tn, fp, FALSE, 0)[1]
  tpr <- calc_tpr(tp, fn, FALSE, 0)[1]

  gm <- calc_g(tpr, tnr)

  if (adjust) {

    neg <- tn + fp
    n <- neg + tp + fn
    neg <- neg / n

    if (tpr == 0) {

      agm <- 0

    } else if (neg == 0) {

      agm <- NA_real_
      warning("Can not calculate adjusted geometric mean. \n
            No negatives in reference.")

    } else {

      gm <- (gm + tnr * neg) / (1 + neg)

    }
  }

  return(gm)

}



#' @describeIn calc_gmean
#'
#' @param tbl `r rox("tbl")`
#' @param adjust Should the GM be adjusted for the proportion of negatives?
#' FALSE by default.
#'
#' @export
#'
calc_gmean.table <- function(tbl, adjust = FALSE, ...) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_gmean(tn, fp, tp, fn, adjust)

}



#' @describeIn calc_gmean
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param adjust Should the GM be adjusted for the proportion of negatives?
#' FALSE by default.
#'
#' @export
#'
calc_gmean.data.frame <- function(
    data,
    prediction, reference,
    adjust = FALSE, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_gmean(tbl, adjust)

}
