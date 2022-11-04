#' Calculate Individual classification success index (icsi).
#'
#' @param tp TP
#' @param fn FN
#' @param fp FP
#'
#' @export
#'
calc_icsi <- function(tp, fn, fp) {

  tpr <- calc_tpr(tp, fn, FALSE, 0)[1]
  ppv <- calc_precision(tp, fp, FALSE, 0)[1]

  tpr + ppv - 1

}
