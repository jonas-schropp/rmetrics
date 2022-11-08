#' Calculate Braun-Blanquet similarity (bbs).
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
calc_bbs.default <- function(tp, ppos, pos) {

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
calc_bbs.table <- function(tbl) {

  tp <- tbl[2,2]
  fp <- tbl[2,1]
  ppos <- tp + fp
  pos <- colSums(tbl)

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
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_bbs(tbl)

}


