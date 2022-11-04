#' Calculate F-score (f).
#'
#' Just a helper function for f1, f2 etc. Returns only the estimate.
#' @param tp Number of true positives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#' @param beta beta coefficient
#'
#' @noRd
#' @keywords Internal
calc_f <- function(tp, fp, fn, beta) {

  ((1 + (beta)^2) * tp) / ((1 + (beta)^2) * tp + fp + (beta^2) * fn)

}



#' Calculate G-measure & G-mean.
#'
#' @param item1 True positive rate (TPR) or True negative rate (TNR) or Positive predictive value (PPV)
#' @param item2 True positive rate (TPR) or True negative rate (TNR) or Positive predictive value (PPV)
#'
#' @noRd
#' @keywords Internal
calc_g <- function(item1, item2) {

  if (item1 != 0 & item2 != 0) {
    sqrt(item1 * item2)
  } else {
    NA_real_
  }

}



#' Calculate Automatic/Manual (am).
#'
#' @param ppos Number of positives in predict vector
#' @param pos Number of actual positives
#'
#' @noRd
#' @keywords Internal
calc_am <- function(ppos, pos) {

  ppos - pos

}



#' Calculate reliability
#'
#' @param racc Random accuracy
#' @param acc Accuracy
#'
#' @noRd
#' @keywords Internal
#'
calc_reliability <- function(racc, acc) {

  if (racc < 1) {
    (acc - racc) / (1 - racc)
  } else {
    NA_real_
  }

}
