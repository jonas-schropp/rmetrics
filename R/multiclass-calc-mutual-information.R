
#' Calculate Mutual information.
#'
#' @param tbl confusion matrix
#'
#' @export
#'
calc_mutual_information <- function(tbl) {

  ppos <- diag(tbl) + (rowSums(tbl) - diag(tbl))
  n <- sum(tbl)

  re <- calc_response_entropy(ppos, n)
  ce <- calc_conditional_entropy(tbl)

  re - ce
}
