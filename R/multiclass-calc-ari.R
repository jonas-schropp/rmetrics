
#' Calculate Adjusted Rand index (ari).
#'
#' @param tbl input confusion matrix of the form `table(prediction, reference)`
#' @param on Total number of observations
#'
#' @export
#'
calc_ari <- function(tbl, on) {

  sumnij <- sum(choose(tbl, 2))
  sumai <- sum(choose(colSums(tbl), 2))
  sumbj <- sum(choose(rowSums(tbl), 2))
  Ntwo <- choose(on, 2)

  abs(
    (sumnij - (sumai*sumbj) / Ntwo) /
      (0.5*(sumai + sumbj) - (sumai*sumbj) / Ntwo)
    )

}
