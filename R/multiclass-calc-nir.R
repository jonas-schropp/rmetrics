
#' Calculate No information rate (nir).
#'
#' @param pos Positives in reference.
#' @param on Overall N
#'
#' @export
#'
calc_nir <- function(pos, on) {

  max(pos) / on

}
