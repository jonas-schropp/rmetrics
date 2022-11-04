#' Calculate Size of Grey Area
#'
#' @param ind Number of indeterminate results.
#' @param tot Total number of trials.
#' @param ci.type Either FALSE if no confidence intervals are desired or one of 'boot', ADD OTHERS. If FALSE overwrites ci.level and boot.
#' @param ci.level A number between 0 and 1 for the levels of the confidence intervals that should be calculated.
#'
#' @export
#'
calc_grey <- function(ind, tot, ci.type, ci.level) {

  calc_prop(ind, tot, ci.type, ci.level)

}
