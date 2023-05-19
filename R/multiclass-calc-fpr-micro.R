#' Calculate FPR Micro.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_fpr_micro <- function(...) UseMethod("calc_fpr_micro")



#' @describeIn calc_fpr_micro
#'
#' @param tn `r rox("tnm")`
#' @param fp `r rox("fpm")`
#'
#' @export
#'
calc_fpr_micro.default <- function(tn, fp, ...) {

  1 - calc_tnr_micro(tn, fp)

}



#' @describeIn calc_fpr_micro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_fpr_micro.table <- function(tbl, ...) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_fpr_micro(tn, fp)

}



#' @describeIn calc_fpr_micro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_fpr_micro.data.frame <- function(
    data,
    prediction,
    reference,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_fpr_micro(tbl)

}

