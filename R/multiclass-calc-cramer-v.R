
#' Calculate Cramer's V.
#'
#' @param tbl The contingency table.
#' @param ci.type FALSE if no ci is requested or one out of "ncchisq" (using noncentral chisquare), "ncchisqadj", "fisher" (using fisher z transformation), "fisheradj" (using fisher z transformation and bias correction).
#' @param ci.level double between 0 and 1
#' @param bias_correct should a bias correction be applied?
#'
#' @references
#' Cramer, H. (1946) Mathematical Methods of Statistics. Princeton University Press
#' Agresti, A. (2002) Categorical Data Analysis. John Wiley & Sons
#'
#' @details
#' A Cramer's V between 0 and 0.3 is considered as weak, 0.3-0.7 as medium and > 0.7 as strong.
#' This implementation is based on code by Andri Signorell <andri@signorell.net> and Michael Smithson <michael.smithson@anu.edu.au> for the package DescTools.
#'
#' @returns
#' A numeric vector with the three elements 'v', 'll' and 'ul'. If no CI is requested, 'll' and 'ul' are NA.
#'
#' @export
#'
calc_cramer_v <- function(tbl,
                          ci.type = c("ncchisq", "ncchisqadj", "fisher", "fisheradj"),
                          ci.level = 0.95,
                          bias_correct = FALSE) {

  chisq.hat <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
  df <- prod(dim(tbl) - 1)
  n <- sum(tbl)

  if (bias_correct) {
    phi.hat <- chisq.hat/n
    v <- as.numeric(
      sqrt(max(0, phi.hat - df/(n - 1)) /
             (min(sapply(dim(tbl), function(i) i - 1/(n - 1) * (i - 1)^2) - 1))))
  } else {
    v <- as.numeric(sqrt(chisq.hat/(n * (min(dim(tbl)) - 1))))
  }

  if (!ci.type) {
    ci <- c(NA, NA)
  } else if (ci.type == "ncchisq") {
    ci <- c(lochi(chisq.hat, df, ci.level)[1],
            hichi(chisq.hat, df, ci.level)[1])
    ci <- unname(sqrt((ci)/(n * (min(dim(tbl)) - 1))))
  } else if (ci.type == "ncchisqadj") {
    ci <- c(lochi(chisq.hat, df, ci.level)[1] + df,
            hichi(chisq.hat, df, ci.level)[1] + df)
    ci <- unname(sqrt((ci)/(n * (min(dim(tbl)) - 1))))
  } else if (ci.type == "fisher") {
    se <- 1/sqrt(n - 3) * qnorm(1 - (1 - ci.level)/2)
    ci <- tanh(atanh(v) + c(-se, se))
  } else if (ci.type == "fisheradj") {
    se <- 1/sqrt(n - 3) * qnorm(1 - (1 - ci.level)/2)
    adj <- 0.5 * v/(n - 1)
    ci <- tanh(atanh(v) + c(-se, se) + adj)
  }

  res <- c(v, max(0, ci[1]), min(1, ci[2]))
  names(res) <- c("V", "ll", "ul")
  res

}
