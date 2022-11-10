#' Calculate Calculate (Unbiased) Overall Random Accuracy.
#'
#' @export
#'
calc_oracc <- function(...) UseMethod("calc_oracc")



#' @describeIn calc_oracc
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#' @param n `r rox("n")`
#' @param unbiased TRUE/FALSE, should unbiased random accuracy be returned? FALSE by default.
#'
#' @export
#'
calc_oracc.default <- function(tp, fp, fn, n) {

  ppos <- tp + fp
  pos <- tp + fn

  if (unbiased) {
    sum(((ppos + pos) / (2 * n))^2)
  } else if(!unbiased) {
    sum((ppos * pos) / (n^2))
  }

}



#' @describeIn calc_oracc
#'
#' @param tbl `r rox("tbl")`
#' @param unbiased TRUE/FALSE, should unbiased random accuracy be returned? FALSE by default.
#'
#' @export
#'
calc_oracc.table <- function(tbl, unbiased = FALSE) {

  n <- sum(tbl)
  ppos <- diag(tbl) + (rowSums(tbl) - diag(tbl))
  pos <- colSums(tbl)

  if (unbiased) {
    sum(((ppos + pos) / (2 * n))^2)
  } else if(!unbiased) {
    sum((ppos * pos) / (n^2))
  }

}



#' @describeIn calc_oracc
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param unbiased TRUE/FALSE, should unbiased random accuracy be returned? FALSE by default.
#'
#' @export
#'
calc_oracc.data.frame <- function(
    data,
    prediction, reference,
    unbiased = FALSE
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_oracc(tbl, unbiased)

}

