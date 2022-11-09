#' Calculate Joint entropy.
#'
#' @export
#'
calc_joint_entropy <- function(...) UseMethod("calc_joint_entropy")



#' @describeIn calc_joint_entropy
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_joint_entropy.table <- function(tbl) {

  p_prime <- tbl / sum(tbl)

  p_prime <- p_prime[p_prime != 0]
  p_prime_log <- log(p_prime, 2)

  -sum(p_prime * p_prime_log)

}



#' @describeIn calc_joint_entropy
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_joint_entropy.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_joint_entropy(tbl)

}
