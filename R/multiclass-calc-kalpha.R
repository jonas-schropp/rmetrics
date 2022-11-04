
#' Calculate Unweighted Krippendorff's alpha (kalpha).
#'
#' @param tbl The contingency table.
#' @param otp Overall true positives.
#'
#' @export
#'
calc_kalpha <- function(tbl, otp) {

  oracc <- calc_oracc(tbl, unbiased = FALSE)

  epsi = 1 / (2 * sum(tbl))
  p_a = (1 - epsi) * calc_oacc(otp, sum(tbl)) + epsi

  calc_reliability(oracc, p_a)

}
