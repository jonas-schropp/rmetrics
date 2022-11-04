
#' Calculate Cross entropy.
#'
#' @param pos vector of actual positives per class
#' @param ppos vector of positives in predict vector per class
#' @param n Total number of observations per class
#'
#' @export
#'
calc_cross_entropy <- function(ppos, pos, n) {

  ref_lik = pos / n    # reference likelihood
  resp_lik = ppos / n  # response likelihood

  resp_lik[resp_lik != 0] <- log(resp_lik[resp_lik != 0], 2)

  -sum(ref_lik * resp_lik)

}
