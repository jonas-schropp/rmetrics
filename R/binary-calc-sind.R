#' Calculate Similarity Index
#'
#' @export
#'
calc_sind <- function(...) UseMethod("calc_sind")



#' @describeIn calc_sind
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_sind.default <- function(tn, fp, tp, fn) {

  dind <- calc_dind(tn, fp, tp, fn)[1]

  1 - (dind / sqrt(2))

}



#' @describeIn calc_sind
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_sind.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_sind(tn, fp, tp, fn)

}



#' @describeIn calc_sind
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_sind.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_sind(tbl)

}
