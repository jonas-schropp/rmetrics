#' Calculate False Discovery Rate
#'
#' The false discovery rate (FDR) is a measure of the proportion of false
#' positive results among all positive results in a statistical test. It
#' is calculated as the number of false positives divided by the total
#' number of positive results.
#'
#' @details
#' To calculate the FDR, the following formula is used:
#'
#' FDR = (Number of false positives) / (Total number of positive results)
#'
#' The resulting value is a proportion, with values closer to 0 indicating a
#' more reliable statistical test and values closer to 1 indicating a less
#' reliable test.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_fdr <- function(...) UseMethod("calc_fdr")



#' @describeIn calc_fdr
#'
#' @param fp `r rox("fp")`
#' @param tp `r rox("tp")`
#' @param ci.type `r rox("prop.ci.type")`
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fdr.default <- function(fp, tp, ci.type, ci.level, ...) {

  calc_prop(fp, fp + tp, ci.type, ci.level)

}



#' @describeIn calc_fdr
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type `r rox("prop.ci.type")`
#' @param ci.level `r rox("ci.level")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_fdr.table <- function(
    tbl,
    ci.type,
    ci.level,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  fp <- tbl[2,1]

  calc_prop(fp, fp + tp, ci.type, ci.level)

}



#' @describeIn calc_fdr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type `r rox("prop.ci.type")`
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_fdr.data.frame <- function(
    data,
    prediction,
    reference,
    ci.type,
    ci.level,
    incr = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_fdr.table(tbl, ci.type, ci.level, incr)

}
