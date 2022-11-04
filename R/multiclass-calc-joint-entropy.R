
#' Calculate Joint entropy.
#'
#' @param tbl input table
#'
#' @export
#'
calc_joint_entropy <- function(tbl) {

  p_prime <- tbl / rowSums(tbl)
  # This might not work if table in pycm is ref-pred rather than pred-ref?
  # if result is different just turn around (colwise)

  p_prime_log <- log(p_prime[p_prime != 0], 2)
  p_prime <- p_prime[p_prime != 0]

  -sum(p_prime * p_prime_log)

}
