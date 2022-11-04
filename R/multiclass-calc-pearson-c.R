
#' Calculate Pearson's C.
#'
#' @param tbl confusion matrix
#' @param on Total number of observations
#'
#' @importFrom stats chisq.test
#'
#' @export
#'
calc_pearson_c <- function(tbl, on) {

  chi <- chisq.test(tbl)$statistic

  sqrt(chi / (on + chi))

}
