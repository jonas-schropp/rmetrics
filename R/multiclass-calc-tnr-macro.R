
#' Calculate TNR macro.
#'
#' @param tn TN by class
#' @param fp FP by class
#'
#' @export
#'
calc_tnr_macro <- function(tn, fp) {

  tnr <- tn / (tn + fp)

  calc_macro(tnr)

}
