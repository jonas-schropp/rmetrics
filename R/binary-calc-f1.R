#' Calculate F1 Score
#'
#' @export
#'
 calc_f1 <- function(...) UseMethod("calc_f1")



#' @describeIn calc_f1
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
 calc_f1.default <- function(tp, fp, fn) {

   calc_f(tp, fp, fn, 1)

 }



#' @describeIn calc_f1
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_f1.table <- function(tbl) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_f(tp, fp, fn, 1)

}



#' @describeIn calc_f1
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_f1.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_f1(tbl)

}
