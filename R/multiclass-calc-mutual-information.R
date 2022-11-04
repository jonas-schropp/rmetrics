
#' Calculate Mutual information.
#'
#' @param tbl confusion matrix
#' @param pos vector of actual positives per class
#' @param ppos vector of positives in predict vector per class
#' @param n Total number of observations
#'
#' @export
#'
calc_mutual_information <- function(tbl, pos, ppos, n) {

  calc_response_entropy(ppos, n) - calc_conditional_entropy(tbl, pos)

}
