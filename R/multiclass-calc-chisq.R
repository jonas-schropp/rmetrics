
#' Calculate Chi-squared.
#'
#' @param tbl confusion matrix
#'
#' @importFrom stats chisq.test
#'
#' @export
#'
calc_chisq <- function(tbl) {

  chisq.test(tbl)$statistic

}
