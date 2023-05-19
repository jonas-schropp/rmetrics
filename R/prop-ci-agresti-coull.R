#' Calculate Agresti Croull CI for proportions
#'
#' @param successes Number of 'successes' in a bernoulli experiment.
#' Numerator of the proportion to draw.
#' @param trials Total number of trials, denominator of the proportion to draw.
#' @param l Levels of the confidence intervals returned by `ci_levels`.
#'
#' @importFrom stats qnorm
#'
#' @noRd
#' @keywords Internal
ci.agresticoull <- function(successes, trials, l){

  if (successes == 0){

    t <- trials + qnorm(2 * l[3]) ^ 2
    p <- (successes + .5 * (qnorm(2 * l[3])) ^ 2) / t
    lw <- 0
    up <- p - qnorm(2 * l[3]) * sqrt(p * (1 - p) / t)

  } else if (successes == trials){

    t <- trials + qnorm(2 * l[3]) ^ 2
    p <- (successes + .5 * (qnorm(2 * l[3])) ^ 2) / t
    lw <- p + qnorm(2 * l[3]) * sqrt(p * (1 - p) / t)
    up <- 1

  } else{

    t <- trials + qnorm(l[3]) ^ 2
    p <- (successes + .5 * (qnorm(l[3])) ^ 2) / t
    lw <- p + qnorm(l[3]) * sqrt(p * (1 - p) / t)
    up <- p - qnorm(l[3]) * sqrt(p * (1 - p) / t)

  }

  return(c(lw, up))
}
