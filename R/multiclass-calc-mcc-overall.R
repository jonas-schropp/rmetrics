
#' Calculate Calculate Overall_MCC.
#'
#' @param tbl Classification matrix.
#' @param ppos Positives in prediction.
#' @param pos Positives in reference.
#'
#' @export
#'
calc_mcc_overall <- function(tbl, ppos, pos) {

  cov_x_y <- sum(diag(tbl) * sum(ppos) - pos * ppos)
  cov_x_x <- sum(ppos * (sum(ppos) - ppos))
  cov_y_y <- sum(pos * (sum(ppos) - pos))

  cov_x_y / (sqrt(cov_y_y * cov_x_x))

}
