
#' Calculate Similarity index (sind).
#'
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_sind <- function(tn, fp, tp, fn) {

  dind <- (calc_dind(tn = tn, fp = fp, tp = tp, fn = fn))[1]

  1 - (dind / sqrt(2))

}
