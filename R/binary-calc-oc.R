
#' Calculate Overlap coefficient (oc)
#'
#' @param tp Number of true positives in the contingency table.
#' @param ppos Number of positives in predict vector
#' @param pos Number of actual positives
#'
#' @export
#'
calc_oc <- function(tp, ppos, pos) {

  if (ppos > 0 & pos > 0) {

    oc <- tp / min(ppos, pos)

  } else {

    oc <- NA_real_

    warning(
      "Can not calculate Overlap coefficient. \n
      No positive cases in prediction or reference.")

  }

  oc

}
