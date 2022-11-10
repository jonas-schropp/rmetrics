#' Calculate Gwet AC1.
#'
#' @export
#'
calc_gwet_ac1 <- function(...) UseMethod("calc_gwet_ac1")



#' @describeIn calc_gwet_ac1
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#' @param n `r rox("n")`
#'
#' @export
#'
calc_gwet_ac1.default <- function(tp, fp, fn, n) {

  pos <- tp + fn
  ppos <- tp + fp
  otp <- sum(tp)

  pc_ac1 <- calc_pc_ac1(pos, ppos, n)
  oacc <- calc_oacc(otp, n)

  calc_reliability(pc_ac1, oacc)

}



#' @describeIn calc_gwet_ac1
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_gwet_ac1.table <- function(tbl) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_gwet_ac1(tp, fp, fn, n)

}



#' @describeIn calc_gwet_ac1
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_gwet_ac1.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_gwet_ac1(tbl)

}

