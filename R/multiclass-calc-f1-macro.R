
#' Calculate F1 macro.
#'
#' @param tp TP by class
#' @param fp FP by class
#' @param fn FN by class
#'
#' @export
#'
calc_f1_macro <- function(tp, fp, fn) {

  f1 <- (2 * tp) / ((2 * tp + fp + 1) * fn)

  calc_macro(f1)

}
