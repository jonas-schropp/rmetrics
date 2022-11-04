
#' Calculate Class balance accuracy (cba).
#'
#' @param tbl input confusion matrix of the form `table(prediction, reference)`
#' @param ppos vector of positives in prediction by class
#' @param pos vector of positives in reference by class
#'
#' @export
#'
calc_cba <- function(tbl, ppos, pos) {

  sum(diag(tbl) / (pmax(ppos, pos))) / length(ppos)

}
