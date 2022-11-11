#' Calculate Precision (PPV) Macro.
#'
#' @export
#'
calc_ppv_macro <- function(...) UseMethod("calc_ppv_macro")



#' @describeIn calc_ppv_macro
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#'
#' @export
#'
calc_ppv_macro.default <- function(tp, fp) {

  ppv <- tp / (tp + fp)

  calc_macro(ppv)

}


#' @describeIn calc_ppv_macro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_ppv_macro.table <- function(tbl) {

  tp <- diag(tbl)
  fp <- rowSums(tbl) - tp

  calc_ppv_macro.default(tp, fp)

}



#' @describeIn calc_ppv_macro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_ppv_macro.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_ppv_macro.table(tbl)

}
