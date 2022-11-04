
#' Calculate Precision macro (ppv macro).
#'
#' @param tp TP by class
#' @param fp FP by class
#'
#' @export
#'
calc_ppv_macro <- function(tp, fp) {

  ppv <- tp / (tp + fp)

  calc_macro(ppv)

}
