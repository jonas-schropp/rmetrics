
#' Calculate FPR macro.
#'
#' @param fp FP by class
#' @param tn TN by class
#'
#' @export
#'
calc_fpr_macro <- function(fp, tn) {

  fpr <- fp / (fp + tn)

  calc_macro(fpr)

}
