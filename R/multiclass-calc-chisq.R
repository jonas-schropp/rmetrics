#' Calculate Chi-squared.
#'
#' A simple wrapper over `stats::chisq.test`
#'
#' @returns
#' A named vector with the test statistic, degrees of freedom and p value of the test.
#'
#' @export
#'
calc_chisq <- function(...) UseMethod("calc_chisq")



#' @describeIn calc_chisq
#'
#' @param tbl `r rox("tbl")`
#' @param ... Additional parameters passed on to `chisq.test`.
#'
#' @importFrom stats chisq.test
#'
#' @export
#'
calc_chisq.table <- function(tbl, ...) {

  t <- chisq.test(tbl, ...)

  res <- c(t$statistic, t$parameter, t$p.value)
  names(res) <- c("chisq", "df", "pval")
  return(res)

}



#' @describeIn calc_chisq
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ... Additional parameters passed on to `chisq.test`.
#'
#' @export
#'
calc_chisq.data.frame <- function(
    data,
    prediction, reference,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_chisq(tbl, ...)

}

