#' Calculate No information Rate.
#'
#' @export
#'
calc_nir <- function(...) UseMethod("calc_nir")



#' @describeIn calc_nir
#'
#' @param tp `r rox("tpm")`
#' @param fn `r rox("fnm")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_nir.default <- function(tp, fn, n) {

  pos <- tp + fn
  max(pos) / n

}



#' @describeIn calc_nir
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_nir.table <- function(tbl) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  n <- sum(tbl)

  calc_nir(tp, fn, n)

}



#' @describeIn calc_nir
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_nir.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_nir(tbl)

}

