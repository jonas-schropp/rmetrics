#' Calculate Braun-Blanquet similarity.
#'
#' The Braun-Blanquet similarity is a measure of the degree to which two sets
#' of observations are similar. It is commonly used in ecology to compare
#' the composition of two different plant communities, but it can also be
#' applied to other types of data sets.
#'
#' @details
#' To calculate the Braun-Blanquet similarity, the following steps are followed:
#'
#' - For each observation in the first set of data, count the number of times it occurs.
#' - For each observation in the second set of data, count the number of times it occurs.
#' - For each observation that occurs in both sets of data, calculate the minimum of the two counts and add this value to the Braun-Blanquet similarity score.
#' - Divide the Braun-Blanquet similarity score by the total number of observations in both sets of data.
#'
#' The resulting value is a measure of the degree to which the two sets of data
#' are similar, with values closer to 1 indicating greater similarity and
#' values closer to 0 indicating less similarity.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_bbs <- function(...) UseMethod("calc_bbs")



#' @describeIn calc_bbs
#'
#' @param tp `r rox("tp")`
#' @param ppos `r rox("ppos")`
#' @param pos `r rox("pos")`
#'
#' @export
#'
calc_bbs.default <- function(tp, ppos, pos, ...) {

  if (ppos > 0 | pos > 0) {

    bbs <- tp / max(ppos, pos)

  } else {

    bbs <- NA_real_

    warning(
      "Can not calculate Braun-Blanquet similarity. \n
      No positive cases in prediction and reference.")

  }

  bbs

}



#' @describeIn calc_bbs
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_bbs.table <- function(tbl, ...) {

  tp <- tbl[2,2]
  ppos <- sum(tbl[2,])
  pos <- sum(tbl[,2])

  calc_bbs(tp, ppos, pos)

}



#' @describeIn calc_bbs
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_bbs.data.frame <- function(
    data,
    prediction, reference, ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_bbs(tbl)

}


