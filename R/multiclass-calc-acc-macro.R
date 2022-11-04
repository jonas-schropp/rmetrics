
#' Calculate Accuracy macro (acc macro).
#'
#' @param tbl input confusion matrix of the form `table(prediction, reference)`
#' @param tp TP by class
#' @param tn TN by class
#'
#' @export
#'
calc_acc_macro <- function(tbl, tp, tn) {

  acc <- (tp + tn) / sum(tbl)

  calc_macro(acc)

}
