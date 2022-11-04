
#' Calculate classification success index macro (csi macro).
#'
#' @param tp TP by class
#' @param fn FN by class
#' @param fp FP by class
#'
#' @export
#'
calc_csi_macro <- function(tp, fn, fp) {

  csi <- double(length(tp))

  for (i in 1:length(tp)) {
    csi[i] <- calc_icsi(tp[i], fn[i], fp[i])
  }

  calc_macro(csi)

}
