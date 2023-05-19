#' Calculate Negative Likelihood Ratio
#'
#' @param ... `r rox("dots")`
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
#' @param ci.type Either `FALSE` if no confidence intervals are desired or
#' 'koopman'. If `FALSE` overwrites `ci.level`.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_nlr.default <- function(
    tp,
    fn,
    fp,
    tn,
    ci.type,
    ci.level,
    ...
    ) {

  neg <- fp + tn
  pos <- tp + fn

  if (neg == 0 | pos == 0) {
    warning("Can not calculate negative likelihood ratio if there are
            no negative or positive cases. Returning NA.")
    res <- c(NA_real_, NA_real_, NA_real_)
    names(res) <- c("nlr", "ll", "ul")
    return(res)
  } else if (tn == 0) {
    warning("Can not calculate negative likelihood ratio if there are
            no true negative cases. Returning NA.")
    res <- c(NA_real_, NA_real_, NA_real_)
    names(res) <- c("nlr", "ll", "ul")
    return(res)
  } else {
    lr <- (1 - tp / pos) / (tn / neg)
  }


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
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_nlr.table <- function(
    tbl,
    ci.type,
    ci.level,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_nlr.default(tp, fn, fp, tn, ci.type, ci.level)

}



#' @describeIn calc_nlr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or
#' 'koopman'. If FALSE overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_nlr.data.frame <- function(
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

  calc_nlr.table(tbl, ci.type, ci.level, incr)

}
