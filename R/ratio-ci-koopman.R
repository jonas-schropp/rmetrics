#' Calculate CI for ratios of proportions by Koopman 1984
#'
#' @param tp Number of true positives
#' @param fn Number of false negatives
#' @param fp Number of false positives
#' @param tn Number of true negatives
#' @param lr Likelihood Ratio
#' @param ci.level Level of ci, between 0 and 1
#' @param pos TRUE for positive likelihood ratio, FALSE for negative
#'
#' @importFrom stats qnorm
#'
#' @noRd
#' @keywords Internal
#'
ci.koopman <- function(tp, fn, fp, tn, lr, ci.level, pos = TRUE) {


  alpha = 1 - ci.level

  # For positive Likelihood Ratio
  if (pos) {

    if ( tp != 0 & fp != 0 ) {

      sigma2 <- (1/tp) - (1/(tp+fn)) + (1/fp) - (1/(fp+tn))

      ll <- lr * exp(-qnorm(1 - (alpha/2)) * sqrt(sigma2))
      ul <- lr * exp(qnorm(1 - (alpha/2)) * sqrt(sigma2))


    } else if ( tp == 0 & fp == 0 ) {

      ll <- 0
      ul <- Inf

    } else if ( tp == 0 & fp != 0 ) {

      tp.temp <- 1/2

      spec.temp <- tn/(fp + tn)
      sens.temp <- tp.temp/(tp + fn)

      lr.temp <- sens.temp/(1 - spec.temp)

      ll <- 0

      sigma2 <- (1/tp.temp) - (1/(tp.temp + fn)) + (1/fp) - (1/(fp + tn))
      ul <- lr.temp * exp(qnorm(1 - (alpha/2))*sqrt(sigma2))

    } else if ( tp != 0 & fp == 0 ) {

      fp.temp <- (1/2)

      spec.temp <- tn/(fp.temp + tn)
      sens.temp <- tp/(tp + fn)

      lr.temp <- sens.temp/(1 - spec.temp)

      sigma2 <- (1/tp) - (1/(tp + fn)) + (1/fp.temp) - (1/(fp.temp + tn))

      ll <- lr.temp * exp(-qnorm(1 - (alpha/2)) * sqrt(sigma2))
      ul <- Inf

    } else if ( (tp == (tp + fn)) & (fp == (fp + tn)) ) {

      tp.temp <- tp - (1/2)
      fp.temp <- fp - (1/2)

      spec.temp <- tn/(fp.temp + tn)
      sens.temp <- tp.temp/(tp + fn)

      lr.temp <- sens.temp/(1 - spec.temp)

      sigma2 <- (1/tp.temp) - (1/(tp.temp + fn)) + (1/fp.temp) - (1/(fp.temp + tn))

      ll <- lr.temp * exp(-qnorm(1-(alpha/2))*sqrt(sigma2))
      ul <- lr.temp * exp(qnorm(1-(alpha/2))*sqrt(sigma2))

    }

    # For negative likelihood ratio
  } else if (!pos) {

    if ( fn != 0 & tn != 0 ) {

      sigma2 <- (1/fn) - (1/(tp + fn)) + (1/tn) - (1/(fp + tn))

      ll <- lr * exp(-qnorm(1 - (alpha/2)) * sqrt(sigma2))
      ul <- lr * exp(qnorm(1 - (alpha/2)) * sqrt(sigma2))

    } else if ( fn == 0 & tn == 0 ) {

      ll <- 0
      ul <- Inf

    } else if ( fn == 0 & tn != 0 ) {

      fn.temp <- 0.5

      spec.temp <- tn/(fp + tn)
      sens.temp <- tp/(tp + fn.temp)

      lr.temp <- (1 - sens.temp) / spec.temp

      sigma2 <- (1 / fn.temp) - (1 / (tp + fn)) + (1 / tn) - (1 / (fp + tn))

      ll <- 0
      ul <- lr.temp * exp(qnorm(1 - (alpha / 2)) * sqrt(sigma2))

    } else if ( fn != 0 & tn == 0 ) {

      tn.temp <- (1/2)

      spec.temp <- tn.temp / (fp + tn)
      sens.temp <- tp / (tp + fn)

      lr.temp <- (1 - sens.temp)/spec.temp

      sigma2 <- (1 / fn) - (1 / (tp + fn)) + (1 / tn.temp) - (1 / (fp + tn))

      ll <- lr.temp * exp(-qnorm(1 - (alpha / 2)) * sqrt(sigma2))
      ul <- Inf

    } else if ( (fn == (tp + fn)) & (tn == (fp + tn)) ) {

      fn.temp <- fn - 0.5
      tn.temp <- tn - 0.5

      spec.temp <- tn.temp/(fp + tn)
      sens.temp <- tp / (tp + fn.temp)

      lr.temp <- (1 - sens.temp)/spec.temp

      sigma2 <- (1 / fn.temp) - (1 / (tp + fn)) + (1 / tn.temp) - (1 / (fp + tn))

      ll <- lr.temp * exp(-qnorm(1 - (alpha / 2)) * sqrt(sigma2))
      ul <- lr.temp * exp(qnorm(1 - (alpha / 2)) * sqrt(sigma2))

    }

  }

  return(c(ll, ul))

}
