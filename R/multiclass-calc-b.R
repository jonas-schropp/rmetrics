
#' Calculate Bangdiwala's B.
#'
#' @param tp vector of true positives per class
#' @param ppos vector of positives in predict vector per class
#' @param pos vector of actual positives per class
#'
#' @export
#'
calc_b <- function(tp, ppos, pos) {

  sum(tp^2) / sum(ppos * pos)

}
