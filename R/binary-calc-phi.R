
#' Calculate Phi Coefficient
#'
#' @param tp Number of true positives in the contingency table.
#' @param tn Number of true negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @export
#'
calc_phi <- function(tp, tn, fp, fn) {

  (tp*tn - fp*fn) / sqrt( (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn) )

}
