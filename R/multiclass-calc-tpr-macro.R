
#' Calculate TPR macro.
#'
#' @param tp TP by class
#' @param fn FN by class
#'
#' @export
#'
calc_tpr_macro <- function(tp, fn) {

  tpr <- tp / (tp + fn)

  calc_macro(tpr)

}
