#' Calculate Positive Likelihood Ratio
#'
#' @param ... `r rox("dots")`
#'
#' @importFrom stats qnorm
#'
#' @source Koopman, PAR (1984) Confidence intervals for the ratio of two binomial proportions. Biometrics; 513-517.
#'
#' @export
#'
calc_plr <- function(...) UseMethod("calc_plr")



#' @describeIn calc_plr
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
calc_plr.default <- function(
    tp, fn, fp, tn,
    ci.type, ci.level,
    ...
    ) {

  tpr <- calc_tpr(tp, fn, F, 0)[1]
  fnr <- calc_fpr(fp, tn, F, 0)[1]

  lr <- tpr / fnr

  # CI method by Koopman (1984). Coverage not that great.
  if (is.na(ci.type) | isFALSE(ci.type)) {
    ci <- c(NA_real_, NA_real_)
  } else if ( ci.type == "koopman") {
    ci <- ci.koopman(tp, fn, fp, tn, lr, ci.level, pos = TRUE)
  } else {
    ci <- c(NA_real_, NA_real_)
  }

  res <- c(lr, ci)
  names(res) <- c("plr", "ll", "ul")
  res

}


#' @describeIn calc_plr
#'
#' @param tbl `r rox("tbl")`
#' @param ci.type Either FALSE if no confidence intervals are desired or 'koopman'. If FALSE overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_plr.table <- function(
    tbl,
    ci.type, ci.level,
    ...) {

  tp <- tbl[2,2]
  tn <- tbl[1,1]
  fp <- tbl[2,1]
  fn <- tbl[1,2]

  calc_plr(tp, fn, fp, tn, ci.type, ci.level)

}



#' @describeIn calc_plr
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param ci.type Either FALSE if no confidence intervals are desired or 'koopman'. If FALSE overwrites ci.level.
#' @param ci.level `r rox("ci.level")`
#'
#' @export
#'
calc_plr.data.frame <- function(
    data,
    prediction, reference,
    ci.type, ci.level,
    ...) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_plr(tbl, ci.type, ci.level)

}
