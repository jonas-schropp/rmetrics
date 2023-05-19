#' Calculate False Omission Rate
#'
#' The false omission rate (FOR) is a measure of the proportion of false
#' negative results among all negative results in a statistical test. It
#' is calculated as the number of false negatives divided by the total number
#' of negative results.
#'
#' @details
#' To calculate the FOR, the following formula is used:
#'
#' FOR = (Number of false negatives) / (Total number of negative results)
#'
#' The resulting value is a proportion, with values closer to 0 indicating a
#' more reliable statistical test and values closer to 1 indicating a less
#' reliable test.
#'
#' The FOR is a useful metric to identify tests that are more likely
#' to accurately identify true negative results.
#'
#' @param ... `r rox("dots")`
#'
#' @export
#'
calc_for <- function(...) UseMethod("calc_for")



#' @describeIn calc_for
#'
#' @param fn `r rox("fn")`
#' @param tn `r rox("tn")`
#' @param ci.type `r rox("prop.ci.type")`
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_for.default <- function(fn, tn, ci.type, ci.level, ...) {

   calc_prop(fn, fn + tn, ci.type, ci.level)

}



#' @describeIn calc_for
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type `r rox("prop.ci.type")`
#' @param ci.level `r rox("ci.level")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_for.table <- function(
    tbl,
    ci.type,
    ci.level,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tn <- tbl[1,1]
  fn <- tbl[1,2]

  calc_prop(fn, fn + tn, ci.type, ci.level)

}



#' @describeIn calc_for
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type `r rox("prop.ci.type")`
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_for.data.frame <- function(
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

  calc_for.table(tbl, ci.type, ci.level, incr)

}
