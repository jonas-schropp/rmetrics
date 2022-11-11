#' Calculate TNR Macro.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_tnr_macro <- function(...) UseMethod("calc_tnr_macro")



#' @describeIn calc_tnr_macro
#'
#' @param tn `r rox("tnm")`
#' @param fp `r rox("fpm")`
#'
#' @export
#'
calc_tnr_macro.default <- function(tn, fp, ...) {

  tnr <- tn / (tn + fp)

  calc_macro(tnr)

}


#' @describeIn calc_tnr_macro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_tnr_macro.table <- function(tbl, ...) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_tnr_macro(tn, fp, ...)

}



#' @describeIn calc_tnr_macro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_tnr_macro.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_tnr_macro(tbl, ...)

}
