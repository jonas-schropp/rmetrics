
#' Calculate Reference Entropy.
#'
#' @param pos vector of actual positives per class
#' @param n Total number of observations
#'
#' @export
#'
calc_reference_entropy <- function(pos, n) {

  calc_entropy(pos, n)

}
