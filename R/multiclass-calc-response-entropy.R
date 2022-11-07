
#' Calculate Response Entropy.
#'
#' @param ppos vector of positives in predict vector per class.
#' @param n Vector of total number of observations by class.
#'
#' @export
#'
calc_response_entropy <- function(ppos, n) {

  calc_entropy(ppos, n)

}
