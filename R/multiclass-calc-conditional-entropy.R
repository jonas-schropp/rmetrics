
#' Calculate conditional entropy.
#'
#' @param tbl confusion matrix
#' @param pos vector of actual positives per class
#'
#' @export
#'
calc_conditional_entropy <- function(tbl, pos) {

  res <- matrix(0, nrow(tbl), ncol(tbl))
  n <- rowSums(tbl)

  for (i in 1:ncol(tbl)){
    tmp <- 0
    for (j in 1:nrow(tbl)){
      p_prime <- 0
      if (pos[i] != 0) p_prime <- tbl[i,j] / pos[i]
      if (p_prime != 0) tmp <- tmp + p_prime * log(p_prime, 2)
      res[i,j] <- tmp * (pos[i] / n[i])
    }
  }

  -sum(res)

}
