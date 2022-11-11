#' Calculate Cramer's V.
#'
#' @references
#' Cramer, H. (1946) Mathematical Methods of Statistics. Princeton University Press
#' Agresti, A. (2002) Categorical Data Analysis. John Wiley & Sons
#'
#' @details
#' A Cramer's V between 0 and 0.3 is considered as weak, 0.3-0.7 as medium and > 0.7 as strong. Equivalent to Tschuprow's T in square tables.
#'
#' @author
#' This implementation is based on code by Andri Signorell <andri@signorell.net> and Michael Smithson <michael.smithson@anu.edu.au> for the package `DescTools` with only minor changes.
#'
#' @returns
#' A numeric vector with the three elements 'v', 'll' and 'ul'. If no CI is requested, 'll' and 'ul' are NA.
#'
#' @export
#'
calc_cramer_v <- function(...) UseMethod("calc_cramer_v")



#' @describeIn calc_cramer_v
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type FALSE if no ci is requested or one out of "ncchisq" (using noncentral chisquare), "ncchisqadj", "fisher" (using fisher z transformation), "fisheradj" (using fisher z transformation and bias correction).
#' @param ci.level `r rox("ci.level")`
#' @param bias.correct Should a bias correction be applied? FALSE by default.
#' @param ... Additional arguments passed on to `stats::chisq.test`. Not used.
#'
#' @export
#'
calc_cramer_v.table <- function(
    tbl,
    ci.type = c("ncchisq", "ncchisqadj", "fisher", "fisheradj"),
    ci.level = 0.95,
    bias.correct = FALSE,
    ...
    ) {

  dims <- dim(tbl)
  chisq.hat <- calc_chisq.table(tbl, correct = FALSE, ...)[1]
  df <- prod(dims - 1)
  n <- sum(tbl)
  sqr <- sapply(dims, function(i) i - 1/(n - 1) * (i - 1)^2)

  if (bias.correct) {
    phi.hat <- chisq.hat/n
    v <- as.numeric(sqrt(max(0, phi.hat - df/(n - 1)) / (min(sqr - 1))))
  } else {
    v <- as.numeric(sqrt(chisq.hat/(n * (min(dims) - 1))))
  }

  if (isFALSE(ci.type)) {
    ci <- c(NA, NA)
  } else if (ci.type == "ncchisq") {
    ci <- c(lochi(chisq.hat, df, ci.level)[1],
            hichi(chisq.hat, df, ci.level)[1])
    ci <- unname(sqrt((ci)/(n * (min(dims) - 1))))
  } else if (ci.type == "ncchisqadj") {
    ci <- c(lochi(chisq.hat, df, ci.level)[1] + df,
            hichi(chisq.hat, df, ci.level)[1] + df)
    ci <- unname(sqrt((ci)/(n * (min(dims) - 1))))
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



#' @describeIn calc_cramer_v
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_cramer_v.data.frame <- function(
    data,
    prediction = "prediction",
    reference = "reference",
    ci.type = c("ncchisq", "ncchisqadj", "fisher", "fisheradj"),
    ci.level = 0.95,
    bias.correct = FALSE,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_cramer_v(tbl, ci.type, ci.level, bias.correct)

}
