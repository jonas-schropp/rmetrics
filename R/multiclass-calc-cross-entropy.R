#' Calculate Cross entropy.
#'
#' @export
#'
calc_cross_entropy <- function(...) UseMethod("calc_cross_entropy")



#' @describeIn calc_cross_entropy
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#'
#' @export
#'
calc_cross_entropy.default <- function(tp, fp, fn) {

  ppos <- tp + fp
  pos <- tp + fn

  ref_lik <- pos / n    # reference likelihood
  resp_lik <- ppos / n  # response likelihood

  resp_lik[resp_lik != 0] <- log(resp_lik[resp_lik != 0], 2)

  -sum(ref_lik * resp_lik)

}



#' @describeIn calc_cross_entropy
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_cross_entropy.table <- function(tbl) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_cross_entropy(tp, fp, fn)

}



#' @describeIn calc_cross_entropy
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_cross_entropy.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_cross_entropy(tbl)

}

