
#' Calculate F2 Score
#'
#' @param tp Number of true positives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_f2 <- function(tp, fp, fn) {

  calc_f(tp, fp, fn, 2)

}
