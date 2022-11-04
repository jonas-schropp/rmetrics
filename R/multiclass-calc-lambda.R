
#' Calculate Goodman Kruskal Lambda.
#'
#' Calculate symmetric and asymmetric Goodman Kruskal lambda and their confidence intervals. Lambda is a measure of proportional reduction in error in cross tabulation analysis. For any sample with a nominal independent variable and dependent variable (or ones that can be treated nominally), it indicates the extent to which the modal categories and frequencies for each value of the independent variable differ from the overall modal category and frequency, i.e. for all values of the independent variable together
#'
#' @param tbl input table
#' @param direction character, either "symmetric", "row" or "column".
#' @param ci.type FALSE if no ci is requested or "normal" for normal approximation CIs
#' @param ci.level double between 0 and 1
#'
#' @references
#' Agresti, A. (2002) Categorical Data Analysis. John Wiley & Sons
#' Goodman, L. A., Kruskal W. H. (1979) Measures of Association for Cross Classifications. New York: Springer-Verlag (contains articles appearing in J. Amer. Statist. Assoc. in 1954, 1959, 1963, 1972).
#' Liebetrau, A. M. (1983) Measures of Association, Sage University Papers Series on Quantitative Applications in the Social Sciences, 07-004. Newbury Park, CA: Sage, pp. 17–24
#'
#' @details
#' This implementation is based on code by Andri Signorell <andri@signorell.net> and Antti Arppe <antti.arppe@helsinki.fi> for the package DescTools.
#'
#' @returns
#' A numeric vector with the three elements 'lambda', 'll' and 'ul'. If no CI is requested, 'll' and 'ul' are NA.
#'
#' @export
#'
calc_lambda <- function (tbl,
                         direction = c("symmetric", "row", "column"),
                         ci.type = FALSE, ci.level = 0.95) {

  n <- sum(tbl)
  csum <- colSums(tbl)
  rsum <- rowSums(tbl)
  rmax <- apply(tbl, 1, max)
  cmax <- apply(tbl, 2, max)
  max.rsum <- max(rsum)
  max.csum <- max(csum)
  nr <- nrow(tbl)
  nc <- ncol(tbl)

  if (direction == "symmetric") {
    res <- 0.5 * (sum(rmax, cmax) - (max.csum + max.rsum)) /
      (n - 0.5 * (max.csum + max.rsum))
  } else if (direction == "row") {
    res <- (sum(cmax) - max.rsum)/(n - max.rsum)
  } else if (direction == "column") {
    res <- (sum(rmax) - max.csum)/(n - max.csum)
  }

  if (!ci.type) {

    ci <- c(NA, NA)

  } else if (ci.type == "normal") {

    if (direction == "symmetric") {

      sigma2 <- lambda.var.symmetric(tbl, n, max.csum, max.rsum,
                                     csum, rsum, nr, nc, cmax, rmax)
    } else if (direction == "row") {
      sigma2 <- lambda.var.row(tbl, n, max.csum, max.rsum,
                               csum, rsum, nr, nc, cmax, rmax)
    } else if (direction == "column") {
      sigma2 <- lambda.var.column(tbl, n, max.csum, max.rsum,
                                  csum, rsum, nr, nc, cmax, rmax)
    }

    pr2 <- 1 - (1 - ci.level)/2

    ci <- pmin(1, pmax(0, qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + res))

  }
  res <- c(res, ci[1], ci[2])
  names(res) <- c("lambda", "ll", "ul")

  return(res)
}
