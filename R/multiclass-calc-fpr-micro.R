
#' Calculate FPR macro.
#'
#' @param tn TN by class
#' @param fp FP by class
#'
#' @export
#'
calc_fpr_micro <- function(tn, fp) {

  1 - calc_tnr_micro(tn, fp)

}
