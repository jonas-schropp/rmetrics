
#' Calculate Random acc Unbiased
#'
#' @param ppos Number of positives in predict vector
#' @param pos Number of positives in reference vector
#' @param n Total number of samples
#'
#' @export
#'
calc_raccu <- function(ppos, pos, n) {

  ((ppos) + (pos)) / (2 * n)^2

}
