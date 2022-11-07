#' Calculate Calculate overall random accuracy or unbiased overall random accuracy (oracc).
#'
#' @param tbl The contingency table.
#' @param unbiased TRUE/FALSE, should unbiased random accuracy be returned? FALSE by default.
#'
#' @export
#'
calc_oracc <- function(tbl, unbiased = FALSE) {

  if (unbiased) {
    sum((rowSums(tbl) * colSums(tbl)) / (sum(tbl))^2)
  } else if(!unbiased) {
    sum((rowSums(tbl) * colSums(tbl)) / (2 * sum(tbl)^2))
  }

}
