#' Calculate Discriminant Power.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_dp <- function(...) UseMethod("calc_dp")



#' @describeIn calc_dp
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_dp.default <- function(tn, fp, tp, fn, ...) {

  tpr <- calc_tpr(tp, fn, F, 0)[1]
  tnr <- calc_tnr(tn, fp, F, 0)[1]

  if (is.na(tpr) | is.na(tnr)) {
    warning("Discriminant power can not be calculated if
             there are no positive of no negative cases. Returning NA.")
    return(NA_real_)
  }

  if (tpr == 1 | tnr == 1) {
    warning("Discriminant power can not be calculated with
             perfect tpr or tnr. Returning NA.")
    return(NA_real_)
  }

  a <- tpr / (1 - tpr)
  b <- tnr / (1 - tnr)

  (sqrt(3) / pi) * (log(a, 10) + log(b, 10))

}



#' @describeIn calc_dp
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_dp.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_dp.default(tn, fp, tp, fn)

}



#' @describeIn calc_dp
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_dp.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_dp.table(tbl, incr)

}
