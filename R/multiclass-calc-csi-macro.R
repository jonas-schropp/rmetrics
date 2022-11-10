#' Calculate classification Success Index Macro.
#'
#' @export
#'
calc_csi_macro <- function(...) UseMethod("calc_csi_macro")



#' @describeIn calc_csi_macro
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#'
#' @export
#'
calc_csi_macro.default <- function(tp, fn, fp) {

  csi <- double(length(tp))
  for (i in 1:length(tp)) csi[i] <- calc_icsi(tp[i], fn[i], fp[i])

  calc_macro(csi)

}



#' @describeIn calc_csi_macro
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_csi_macro.table <- function(tbl) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_csi_macro(tp, fp, fn)

}



#' @describeIn calc_csi_macro
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_csi_macro.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_csi_macro(tbl)

}

