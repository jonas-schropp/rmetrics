
#' Calculate Global performance index (rr).
#'
#' @param ppos Positives in prediction.
#'
#' @export
#'
calc_rr <- function(ppos) {

  sum(ppos) / length(ppos)

}
