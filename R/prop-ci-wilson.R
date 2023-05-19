#' Calculate Wilson CI for proportions
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
ci.wilson <- function(successes, trials, l){

  p <- successes/trials
  c <- l[2] - l[1]

  if (successes == 0){

    z <- qnorm(c)
    z_sq <- z ^ 2
    ll <- 0
    ul <- (p + z_sq / (2 * trials) +
             z * sqrt(p * (1 - p) /
                        trials + z_sq /
                        (4 * trials^2))) /
              (1 + z_sq / trials)
    ci <- c(ll, ul)

  } else if (successes == trials){

    z <- qnorm(c)
    z_sq <- z ^ 2
    ll <- (p + z_sq / (2 * trials) -
             z * sqrt(p * (1 - p) /
                        trials + z_sq /
                        (4 * trials^2))) /
      (1 + z_sq / trials)
    ul <- 1
    ci <- c(ll, ul)

  } else{

    z <- qnorm(l[1])
    z_sq <- z ^ 2
    ci <- (p +
             z_sq / (2 * trials) +
             c(1, -1) * z * sqrt(p * (1 - p) / trials + z_sq / (4 * trials^2))) /
      (1 + z_sq / trials)

  }

  return(ci)
}
