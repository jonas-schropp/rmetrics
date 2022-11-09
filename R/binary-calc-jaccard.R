#' Calculate Jaccard index
#'
#' @export
#'
calc_jaccard <- function(...) UseMethod("calc_jaccard")



#' @describeIn calc_jaccard
#'
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param fp `r rox("fp")`
#'
#' @export
#'
calc_jaccard.default <- function(tp, fn, fp) {

  ppos <- tp + fp
  pos <- fn + tp

  tp / (ppos + pos - tp)

}



#' @describeIn calc_jaccard
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_jaccard.table <- function(tbl) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_jaccard(tp, fn, fp)

}



#' @describeIn calc_jaccard
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_jaccard.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_jaccard(tbl)

}
