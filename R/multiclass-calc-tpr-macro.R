#' Calculate TPR Macro.
#'
#' @export
#'
calc_tpr_macro <- function(...) UseMethod("calc_tpr_macro")



#' @describeIn calc_tpr_macro
#'
#' @param tp `r rox("tpm")`
#' @param fn `r rox("fnm")`
#'
#' @export
#'
calc_tpr_macro.default <- function(tp, fn) {

  tpr <- tp / (tp + fn)

  calc_macro(tpr)

}


#' @describeIn calc_tpr_macro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_tpr_macro.table <- function(tbl) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp

  tpr <- tp / (tp + fn)

  calc_macro(tpr)

}



#' @describeIn calc_tpr_macro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_tpr_macro.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_tpr_macro.table(tbl)

}
