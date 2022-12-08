#' Calculate Distance Index.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_dind <- function(...) UseMethod("calc_dind")



#' @describeIn calc_dind
#'
#' @param tn `r rox("tn")`
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#'
#' @export
#'
calc_dind.default <- function(tn, fp, tp, fn, ...) {

   tnr <- calc_tnr(tn, fp, FALSE, 0)[1]
   tpr <- calc_tpr(tp, fn, FALSE, 0)[1]

   sqrt(((1 - tnr)^2) + ((1 - tpr)^2))

 }



#' @describeIn calc_dind
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_dind.table <- function(tbl, ...) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_dind(tn, fp, tp, fn)

}



#' @describeIn calc_dind
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_dind.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_dind(tbl)

}

