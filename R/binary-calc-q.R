#' Calculate Yule's Q.
#'
#' @export
#'
calc_q <- function(...) UseMethod("calc_q")



#' @describeIn calc_q
#'
#' @param tp `r rox("tp")`
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_q.default <- function(tp, tn, fp, fn) {

  if (fp != 0 & fn != 0) {

    or <- (tp * tn) / (fp * fn)
    q <- (or - 1) / (or + 1)

  } else {

    q <- NA_real_
    warning("Can not calculate Yule's Q. \n
            No FP or FN. Returning NA.")

  }

  q

}



#' @describeIn calc_q
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_q.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_q(tp, tn, fp, fn)

}



#' @describeIn calc_q
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_q.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_q(tbl)

}
