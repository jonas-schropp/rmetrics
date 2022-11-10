#' Calculate FNR macro.
#'
#' @export
#'
calc_fnr_macro <- function(...) UseMethod("calc_fnr_macro")



#' @describeIn calc_fnr_macro
#'
#' @param fn `r rox("fnm")`
#' @param tp `r rox("tpm")`
#'
#' @export
#'
calc_fnr_macro.default <- function(fn, tp) {

  fnr <- fn / (fn + tp)

  return(calc_macro(fnr))

}



#' @describeIn calc_fnr_macro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_fnr_macro.table <- function(tbl) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_fnr_macro(fn, tp)

}



#' @describeIn calc_fnr_macro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_fnr_macro.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_fnr_macro(tbl)

}

