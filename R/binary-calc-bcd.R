
#' Calculate Bray-Curtis dissimilarity (bcd).
#'
#' @param ppos Number of positives in predict vector
#' @param pos Number of actual positives
#' @param n Total number of samples.
#'
#' @export
#'
calc_bcd <- function(ppos, pos, n) {

  am <- calc_am(ppos = ppos, pos = pos)

  abs(am) / (2 * n)

}
