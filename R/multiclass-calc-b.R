#' Calculate Bangdiwala's B.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_b <- function(...) UseMethod("calc_b")



#' @describeIn calc_b
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#'
#' @export
#'
calc_b.default <- function(tp, fp, fn, ...) {

  ppos <- tp + fp
  pos <- tp + fn

  sum(tp^2) / sum(ppos * pos)

}



#' @describeIn calc_b
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_b.table <- function(tbl, ...) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_b(tp, fp, fn)

}



#' @describeIn calc_b
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_b.data.frame <- function(
    data,
    prediction,
    reference,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_b(tbl)

}

