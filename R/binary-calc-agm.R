
#' Calculate Adjusted geometric mean (agm).
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_agm <- function(tn, fp, tp, fn) {

  tnr <- calc_tnr(tn, fp, FALSE, 0)[1]
  tpr <- calc_tpr(tp, fn, FALSE, 0)[1]
  gm <- calc_g(tnr, tpr)

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

    agm <- (gm + tnr * neg) / (1 + neg)

  }

  agm

}
