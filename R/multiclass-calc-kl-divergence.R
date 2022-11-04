
#' Calculate Kullback-Liebler (KL) divergence.
#'
#' @param pos vector of actual positives per class
#' @param ppos vector of positives in predict vector per class
#' @param n Total number of observations
#'
#' @export
#'
calc_kl_divergence <- function(pos, ppos, n) {

  sum((pos / n) * log(((pos / n) / (ppos / n)), 2))

}
