#' Calculate FPR Macro.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_fpr_macro <- function(...) UseMethod("calc_fpr_macro")



#' @describeIn calc_fpr_macro
#'
#' @param fp `r rox("fpm")`
#' @param tn `r rox("tnm")`
#'
#' @export
#'
calc_fpr_macro.default <- function(fp, tn, ...) {

  fpr <- fp / (fp + tn)

  calc_macro(fpr)

}



#' @describeIn calc_fpr_macro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_fpr_macro.table <- function(tbl, ...) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_fpr_macro(fp, tn)

}



#' @describeIn calc_fpr_macro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_fpr_macro.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_fpr_macro(tbl)

}

