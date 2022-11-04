
#' Calculate Yule's Q. .
#'
#' @param tp Number of true positives in the contingency table.
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_q <- function(tp, tn, fp, fn) {

  if (fp != 0 & fn != 0) {

    or <- (tp * tn) / (fp * fn)
    q <- (or - 1) / (or + 1)

  } else {

    q <- NA_real_
    warning("Can not calculate Yule's Q. \n
            No FP or FN.")

  }

  q

}
