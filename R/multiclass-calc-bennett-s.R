#' Calculate Bennett's S.
#'
#' @export
#'
calc_bennett_s <- function(...) UseMethod("calc_bennett_s")



#' @describeIn calc_bennett_s
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_bennett_s.table <- function(tbl) {

  pc_s <- calc_pc_s(tbl)
  otp <- sum(diag(tbl)) # Add agreement matrix option here
  n <- sum(tbl)

  oacc <- calc_oacc(otp, n)[1]

  calc_reliability(pc_s, oacc)

}



#' @describeIn calc_bennett_s
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_bennett_s.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_bennett_s(tbl)

}

