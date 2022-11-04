
#' Calculate Positive Likelihood Ratio (plr)
#'
#' @param tp Number of true positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param tn Number of true negatives in the contingency table.
#' @param ci.type Either FALSE if no confidence intervals are desired or 'koopman'. If FALSE overwrites ci.level and boot.
#' @param ci.level A number between 0 and 1 for the levels of the confidence intervals that should be calculated.
#'
#' @importFrom stats qnorm
#'
#' @source Koopman, P. A. R. Confidence intervals for the ratio of two binomial proportions. Biometrics (1984): 513-517.
#'
#' @export
#'
calc_plr <- function(tp, fn, fp, tn, ci.type, ci.level) {

  lr <- (tp / (tp + fn)) / (fn / (fn + tn))

  # CI method by Koopman (1984). Coverage not that great.
  if (is.na(ci.type)) {
    ci <- c(NA_real_, NA_real_)
  } else if ( ci.type == "koopman") {
    ci <- ci.koopman(tp, fn, fp, tn, lr, ci.level, pos = TRUE)
  } else {
    ci <- c(NA_real_, NA_real_)
  }

  c(lr, ci)

}
