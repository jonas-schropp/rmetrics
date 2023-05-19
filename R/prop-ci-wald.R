#' Calculate Normal / Wald approximation for CI for proportions
#'
#' @param successes Number of 'successes' in a bernoulli experiment. Numerator
#' of the proportion to draw.
#' @param trials Total number of trials, denominator of the proportion to draw.
#' @param l Levels of the confidence intervals returned by `ci_levels`.
#'
#' @importFrom stats qnorm
#'
#' @noRd
#' @keywords Internal
ci.wald <- function(successes, trials, l){

  p <- successes / trials

  if (successes == 0){

    lw <- 0
    up <- p - qnorm(l[3]) * sqrt(p * (1 - p) / trials)

  } else if (successes == trials){

    lw <- p + qnorm(l[3]) * sqrt(p * (1 - p) / trials)
    up <- 1

  } else{

    lw <- p + qnorm(l[3]) * sqrt(p * (1 - p) / trials)
    up <- p - qnorm(l[3]) * sqrt(p * (1 - p) / trials)

  }

  return(c(lw, up))
}
