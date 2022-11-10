#' Calculate Class Balance Accuracy.
#'
#' @export
#'
calc_cba <- function(...) UseMethod("calc_cba")



#' @describeIn calc_cba
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#'
#' @export
#'
calc_cba.default <- function(tp, fp, fn) {

  ppos <- tp + fp
  pos <- tp + fn

  sum(tp / (pmax(ppos, pos))) / length(ppos)

}



#' @describeIn calc_cba
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_cba.table <- function(tbl) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  ppos <- tp + fp
  pos <- tp + fn

  sum(tp / (pmax(ppos, pos))) / length(ppos)

}



#' @describeIn calc_cba
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_cba.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_cba(tbl)

}

