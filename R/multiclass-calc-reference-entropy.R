#' Calculate Reference Entropy.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_reference_entropy <- function(...) UseMethod("calc_reference_entropy")



#' @describeIn calc_reference_entropy
#'
#' @param tp `r rox("tpm")`
#' @param fn `r rox("fnm")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_reference_entropy.default <- function(tp, fn, n, ...) {

  pos <- tp + fn

  calc_entropy(pos, n)

}


#' @describeIn calc_reference_entropy
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_reference_entropy.table <- function(tbl, ...) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  n <- sum(tbl)

  calc_reference_entropy(tp, fn, n, ...)

}



#' @describeIn calc_reference_entropy
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_reference_entropy.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_reference_entropy(tbl, ...)

}
