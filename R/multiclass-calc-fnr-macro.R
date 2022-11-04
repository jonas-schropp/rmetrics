
#' Calculate FNR macro.
#'
#' @param fn FN by class
#' @param tp TP by class
#'
#' @export
#'
calc_fnr_macro <- function(fn, tp) {

  fnr <- fn / (fn + tp)

  calc_macro(fnr)

}
