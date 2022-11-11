#' Calculate Global Performance Index.
#'
#' @export
#'
calc_rr <- function(...) UseMethod("calc_rr")



#' @describeIn calc_rr
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#'
#' @export
#'
calc_rr.default <- function(tp, fp) {

  ppos <- tp + fp

  sum(ppos) / length(ppos)

}


#' @describeIn calc_rr
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_rr.table <- function(tbl) {

  tp <- diag(tbl)
  fp <- rowSums(tbl) - tp

  calc_rr(tp, fp)

}



#' @describeIn calc_rr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_rr.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_rr(tbl)

}
