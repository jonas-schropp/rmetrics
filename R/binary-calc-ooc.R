
#' Calculate Otsuka-Ochiai coefficient (ooc)
#'
#' @param tp Number of true positives in the contingency table.
#' @param ppos Number of positives in predict vector
#' @param pos Number of actual positives
#'
#' @export
#'
calc_ooc <- function(tp, ppos, pos) {

  if (ppos > 0 & pos > 0) {

    ooc <- tp / sqrt(ppos * pos)

  } else {

    ooc <- NA_real_

    warning(
      "Can not calculate Otsuka-Ochiai coefficient. \n
      No positive cases in prediction or reference.")

  }

  ooc

}
