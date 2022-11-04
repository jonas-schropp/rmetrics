
#' Calculate Jaccard index (ji).
#'
#' @param tp Number of true positives in the contingency table.
#' @param ppos Number of positives in predict vector
#' @param pos Number of actual positives
#'
#' @export
#'
calc_ji <- function(tp, ppos, pos) {

  tp / (ppos + pos - tp)

}
