#' Calculate Lift Score
#'
#' @export
#'
calc_lift <- function(...) UseMethod("calc_lift")



#' @describeIn calc_lift
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param pos `r rox("pos")`
#' @param neg `r rox("neg")`
#'
#' @export
#'
calc_lift.default <- function(tp, fp, pos, neg) {

  precision <- calc_precision(tp, fp, FALSE, 0)[1]
  prevalence <- calc_prevalence(pos, neg, FALSE, 0)[1]

  precision / prevalence

}



#' @describeIn calc_lift
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_lift.table <- function(tbl) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  pos <- tbl[1,]
  neg <- tbl[2,]

  calc_lift(tp, fp, pos, neg)

}



#' @describeIn calc_lift
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_lift.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_lift(tbl)

}
