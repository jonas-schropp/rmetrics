#' Calculate Exact CI for proportions
#'
#' @param successes Number of 'successes' in a bernoulli experiment. Numerator
#' of the proportion to draw.
#' @param trials Total number of trials, denominator of the proportion to draw.
#' @param l Levels of the confidence intervals returned by `ci_levels`.
#'
#' @importFrom stats qbeta
#'
#' @noRd
#' @keywords Internal
ci.exact <- function(successes, trials, l){

  if (successes == 0){

    lw <- 0
    up <- 1 - l[3] ^ (1/trials)

  } else if (successes == trials){

    lw <- l[3] ^ (1/trials)
    up <- 1

  } else{

    lw <- qbeta(l[1], successes, trials - successes + 1, lower.tail = T)
    up <- qbeta(l[2], successes + 1, trials - successes, lower.tail = T)

  }

  return(c(lw, up))
}
