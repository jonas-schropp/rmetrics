
#' Calculate Response Entropy.
#'
#' @param ppos vector of positives in predict vector per class
#' @param n Total number of observations
#'
#' @export
#'
calc_response_entropy <- function(ppos, n) {

  calc_entropy(ppos, n)

}
