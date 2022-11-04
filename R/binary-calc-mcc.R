
#' Calculate Matthews correlation coefficient (mcc).
#'
#' @param tp Number of true positives in the contingency table.
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_mcc <- function(tp, tn, fp, fn) {

  denom <- (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)

  if (denom != 0) {

    mcc <- (tp*tn - fp*fn) / sqrt(denom)

  } else {

    mcc <- NA_real_

    warning("Can not calculate Matthews correlation coefficient.")

  }

  mcc

}
