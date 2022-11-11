#' Calculate F macro.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_f_macro <- function(...) UseMethod("calc_f_macro")



#' @describeIn calc_f_macro
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#' @param beta Scaling factor. 1 by default for the F1-Score.
#'
#' @export
#'
calc_f_macro.default <- function(tp, fp, fn, beta = 1, ...) {

  f <- calc_f(tp, fp, fn, beta)

  calc_macro(f)

}



#' @describeIn calc_f_macro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_f_macro.table <- function(tbl, beta = 1, ...) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_f_macro(tp, fp, fn, beta)

}



#' @describeIn calc_f_macro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_f_macro.data.frame <- function(
    data,
    prediction, reference,
    beta = 1, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_f_macro(tbl, beta)

}


