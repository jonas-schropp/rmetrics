#' Calculate Accuracy Macro.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_acc_macro <- function(...) UseMethod("calc_acc_macro")



#' @describeIn calc_acc_macro
#'
#' @param tp `r rox("tpm")`
#' @param tn `r rox("tnm")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_acc_macro.default <- function(tp, tn, n, ...) {

  acc <- (tp + tn) / n

  calc_macro(acc)

}



#' @describeIn calc_acc_macro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_acc_macro.table <- function(tbl, ...) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  n <- sum(tbl)
  tn <- n - tp - fn - fp

  calc_acc_macro(tp, tn, n)

}



#' @describeIn calc_acc_macro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_acc_macro.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_acc_macro(tbl)

}

