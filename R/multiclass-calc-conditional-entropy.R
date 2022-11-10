#' Calculate conditional entropy.
#'
#' Computes the average conditional entropy between two vectors.
#'
#' @export
#'
calc_conditional_entropy <- function(...) UseMethod("calc_conditional_entropy")



#' @describeIn calc_conditional_entropy
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_conditional_entropy.data.frame <- function(
    data,
    prediction = "prediction",
    reference = "reference"
    ) {

  tbl <- table(data[, c(prediction, reference)])

  calc_conditional_entropy(tbl)

}



#' @describeIn calc_conditional_entropy
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_conditional_entropy.table <- function(tbl) {

  n <- sum(tbl)
  tbl <- t(tbl)
  pos <- rowSums(tbl)

  p_prime <- tbl / pos
  p_prime[is.infinite(p_prime)] <- 0
  tmp <- p_prime * log(p_prime, 2)

  res <- tmp * (pos / n)

  return(-sum(res))

}
