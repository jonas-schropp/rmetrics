#' Calculate Proportion and corresponding CI analytically
#'
#' Internal function that calculates the proportion and then provides a (analytical) CI.
#'
#' @param successes Number of 'successes' in a bernoulli experiment. Numerator of the proportion to draw.
#' @param trials Total number of trials, denominator of the proportion to draw.
#' @param ci.type Either FALSE if no confidence intervals are desired or one of "agresti.coull", "agresti-coull", "ac", "asymptotic", "normal", "wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes", and "wilson". If FALSE, overwrites ci.level.
#' @param ci.level A number between 0 and 1 for the levels of the confidence intervals that should be calculated.
#'
#' @noRd
#' @keywords Internal
#'
#' @examples
#' calc_prop(5, 30, "wald", 0.95)
#'
calc_prop <- function(
    successes,
    trials,
    ci.type,
    ci.level
) {


  ## Check 'successes ' and 'trials'
  if (missing(successes )) stop("'successes' is missing")
  if (missing(trials)) stop("'trials' is missing")
  if (length(successes ) != 1 && length(trials) != 1)
    stop("'successes ' and 'trials' must both be of length 1")
  if (missing(ci.type)) stop("'ci.type' is missing")
  if (missing(ci.level)) stop("'ci.level' is missing")

  estimate <- successes / trials

  l <- ci_levels(successes, trials, ci.level)

  # Pass on
  if (is.na(ci.type)) {
    ci <- c(NA_real_, NA_real_)
  } else if (ci.type %in% c("agresti.coull", "agresti-coull", "ac")) {
    ci <- ci.agresticoull(successes, trials, l)
  } else if (ci.type %in% c("asymptotic", "normal", "wald")) {
    ci <- ci.wald(successes, trials, l)
  } else if (ci.type %in% c("clopper-pearson", "cp", "exact")) {
    ci <- ci.exact(successes, trials, l)
  } else if (ci.type %in% c("jeffreys", "bayes")) {
    ci <- ci.jeffreys(successes, trials, l)
  } else if (ci.type == "wilson") {
    ci <- ci.wilson(successes, trials, l)
  } else if (ci.type %in% c("boot-percent", "boot-perc", "boot-student",
                            "boot-basic", "boot-normal", "boot-bca",
                            "perc", "basic", "bca")) {
    # ci with boot is calculated differently to save execution time and memory
    # so nothing happens here

    ci <- c(NA_real_, NA_real_)
  } else if (is.null(ci.type) | isFALSE(ci.type)) {
    ci <- c(NA_real_, NA_real_)
  } else {
    warning("Unknown CI type, default to NA.")
  }

  return(c(estimate, ci))

}


#' Transforms ci.level into the levels used internally for CI calculation
#'
#' @param successes Number of 'successes' in a bernoulli experiment. Numerator of the proportion to draw.
#' @param trials Total number of trials, denominator of the proportion to draw.
#' @param ci.level A number between 0 and 1 for the levels of the confidence intervals that should be calculated.
#'
#' @examples
#' ci_levels(5, 20, 0.95)
#'
#' @noRd
#' @keywords Internal
ci_levels <- function(successes, trials, ci.level){

  if (successes == 0){

    level <- c(0, ci.level)

  } else if (successes == trials){

    level <- c(1 - ci.level, 1)

  } else {

    level <- c((1 - ci.level) / 2, 1 - (1 - ci.level) / 2)

  }

  return(c(level, (1 - ci.level) / 2))

}
