
#' Calculate Net Benefit
#'
#' @param tp Number of true positives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param n Total number of samples
#' @param w weight
#'
#' @export
#'
calc_net_benefit <- function(tp, fp, n, w) {

  (tp - w * fp) / n

}
