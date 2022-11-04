
#' Calculate Fowlkes–Mallows index
#'
#' @param tp Number of true positives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_fmi <- function(tp, fp, fn) {

  sqrt( (tp / (tp+fp)) * (tp / (tp+fn)) )

}
