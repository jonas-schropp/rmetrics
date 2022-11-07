
#' Calculate Joint entropy.
#'
#' @param tbl input table
#'
#' @export
#'
calc_joint_entropy <- function(tbl) {

  p_prime <- tbl / sum(tbl)

  p_prime <- p_prime[p_prime != 0]
  p_prime_log <- log(p_prime, 2)

  -sum(p_prime * p_prime_log)

}
