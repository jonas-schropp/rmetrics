#' Calculate Jeffrey's CI for proportions
#'
#' @param successes Number of 'successes' in a bernoulli esuccessesperiment. Numerator of the proportion to draw.
#' @param trials Total number of trials, denominator of the proportion to draw.
#' @param l Levels of the confidence intervals returned by `ci_levels`.
#'
#' @importFrom stats qbeta
#'
#' @noRd
#' @keywords Internal
ci.jeffreys <- function(successes, trials, l){

  lw <- qbeta(l[1], successes + .5, trials - successes + .5)
  up <- qbeta(l[2], successes + .5, trials - successes + .5)

  return(c(lw, up))
}
