
#' Calculate Random accuracy
#'
#' @param ppos Number of positives in predict vector
#' @param pos Number of positives in reference vector
#' @param n Total number of samples
#'
#' @export
#'
calc_racc <- function(ppos, pos, n) {

  (ppos * pos) / (n^2)

}
