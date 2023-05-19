#' Calculate G-measure & G-mean.
#'
#' @param item1 True positive rate (TPR) or True negative rate (TNR) or Positive predictive value (PPV)
#' @param item2 True positive rate (TPR) or True negative rate (TNR) or Positive predictive value (PPV)
#'
#' @noRd
#' @keywords Internal
calc_g <- function(item1, item2) {

  if (is.na(item1) | is.na(item2)) {
    return(NA_real_)
  } else if (item1 == 0 | item2 == 0) {
    return(NA_real_)
  } else {
    return(sqrt(item1 * item2))
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
