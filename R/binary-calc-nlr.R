#' Calculate Negative Likelihood Ratio
#'
#' @importFrom stats qnorm
#'
#' @source Koopman, PAR (1984) Confidence intervals for the ratio of two binomial proportions. Biometrics; 513-517.
#'
#' @export
#'
calc_nlr <- function(...) UseMethod("calc_nlr")



#' @describeIn calc_nlr
#'
#' @param tp `r rox("tp")`
#' @param fn `r rox("fn")`
#' @param fp `r rox("fp")`
#' @param tn `r rox("tn")`
#' @param ci.type Either FALSE if no confidence intervals are desired or 'koopman'. If FALSE overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_nlr.default <- function(tp, fn, fp, tn, ci.type, ci.level) {

  lr <- (1 - (tp / (tp + fn))) / (tn / (fp + tn))

  # CI method by Koopman (1984). Coverage not that great.
  if (is.na(ci.type)) {

    ci <- c(NA_real_, NA_real_)

  } else if (isFALSE(ci.type)) {

    ci <- c(NA_real_, NA_real_)

  } else if (ci.type == "koopman") {

    ci <- ci.koopman(tp, fn, fp, tn, lr, ci.level, pos = FALSE)

  } else {

    ci <- c(NA_real_, NA_real_)

  }

  res <- c(lr, ci)
  names(res) <- c("nlr", "ll", "ul")
  return(res)

}



#' @describeIn calc_nlr
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or 'koopman'. If FALSE overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_nlr.table <- function(tbl, ci.type, ci.level) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_nlr(tp, fn, fp, tn, ci.type, ci.level)

}



#' @describeIn calc_nlr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or 'koopman'. If FALSE overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_nlr.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_nlr(tbl, ci.type, ci.level)

}
