#' Calculate TNR Micro.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_tnr_micro <- function(...) UseMethod("calc_tnr_micro")



#' @describeIn calc_tnr_micro
#'
#' @param tn `r rox("tnm")`
#' @param fp `r rox("fpm")`
#'
#' @export
#'
calc_tnr_micro.default <- function(tn, fp, ...) {

  calc_micro(tn, fp)

}


#' @describeIn calc_tnr_micro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_tnr_micro.table <- function(tbl, ...) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_micro(tn, fp)

}



#' @describeIn calc_tnr_micro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_tnr_micro.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_tnr_micro(tbl, ...)

}
