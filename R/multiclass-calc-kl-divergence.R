#' Calculate Kullback-Leibler Divergence.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_kl_divergence <- function(...) UseMethod("calc_kl_divergence")



#' @describeIn calc_kl_divergence
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#' @param n `r rox("n")`
#' @param epsilon Continuity correction for zero cells. By default 0.000001.
#'
#' @export
#'
calc_kl_divergence.default <- function(
    tp, fp, fn, n,
    epsilon = 0.000001,
    ...) {

  posprop <- (tp + fn) / n + epsilon
  pposprop <- (tp + fp) / n + epsilon

  if (sum(pposprop == 0) == 0) {
    return(sum(posprop * log((posprop / pposprop), 2)))
  } else {

    for (i in 1:length(posprop)) {
      if (pposprop[i] > 0) {
        res[i] <- posprop[i] * log((posprop[i] / pposprop[i]), 2)
      } else if (pposprop[i] == 0 & posprop[i] == 0) {
        res[i] <- 0
      } else {
        warning(
          "Can not calculate Kullback-Leibler divergence due to zero-entries \n
      in predicted classes. Returning NA. \n
      You might want to set epsilon to a number > 0."
        )
      }
      return(sum(res))
    }
  }
}



#' @describeIn calc_kl_divergence
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_kl_divergence.table <- function(
    tbl, epsilon = 0.000001,
    ...) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  n <- sum(tbl)

  calc_kl_divergence(tp, fp, fn, n, epsilon, ...)

}



#' @describeIn calc_kl_divergence
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_kl_divergence.data.frame <- function(
    data,
    prediction, reference,
    epsilon = 0.000001, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_kl_divergence(tbl, epsilon, ...)

}

