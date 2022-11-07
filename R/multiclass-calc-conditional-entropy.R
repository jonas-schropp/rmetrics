
#' Calculate conditional entropy.
#'
#' @param tbl confusion matrix
#' @param pos vector of actual positives per class
#'
#' @export
#'
calc_conditional_entropy <- function(tbl, pos) {

  res <- matrix(0, nrow(tbl), ncol(tbl))
  n <- sum(tbl)

  p_prime <- tbl / pos
  tmp <- p_prime * log(p_prime, 2)

  res <- tmp * (pos / n)

  -sum(res)

}
