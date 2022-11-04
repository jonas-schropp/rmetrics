
#' Calculate Braun-Blanquet similarity (bbs).
#'
#' @param tp Number of true positives in the contingency table.
#' @param ppos Number of positives in predict vector
#' @param pos Number of actual positives
#'
#' @export
#'
calc_bbs <- function(tp, ppos, pos) {

  if (ppos > 0 | pos > 0) {

    bbs <- tp / max(ppos, pos)

  } else {

    bbs <- NA_real_

    warning(
      "Can not calculate Braun-Blanquet similarity. \n
      No positive cases in prediction and reference.")

  }

  bbs

}
