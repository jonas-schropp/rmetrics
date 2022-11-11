#' Calculate Response Entropy.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_response_entropy <- function(...) UseMethod("calc_response_entropy")



#' @describeIn calc_response_entropy
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_response_entropy.default <- function(tp, fp, n, ...) {

  ppos <- fp + tp

  calc_entropy(ppos, n)

}


#' @describeIn calc_response_entropy
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_response_entropy.table <- function(tbl, ...) {

  tp <- diag(tbl)
  fp <- rowSums(tbl) - tp
  n <- sum(tbl)

  calc_response_entropy(tp, fp, n, ...)

}



#' @describeIn calc_response_entropy
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_response_entropy.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_response_entropy(tbl, ...)

}
