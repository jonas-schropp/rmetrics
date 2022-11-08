#' Calculate Calculate overall random accuracy or unbiased overall random accuracy (oracc).
#'
#' @param tbl The contingency table.
#' @param unbiased TRUE/FALSE, should unbiased random accuracy be returned? FALSE by default.
#'
#' @export
#'
calc_oracc <- function(tbl, unbiased = FALSE) {

  n <- sum(tbl)
  ppos <- diag(tbl) + (rowSums(tbl) - diag(tbl))
  pos <- colSums(tbl)

  if (unbiased) {
    sum(((ppos + pos) / (2 * n))^2)
  } else if(!unbiased) {
    sum((ppos * pos) / (n^2))
  }

}
