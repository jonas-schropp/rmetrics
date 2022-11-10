#' Calculate Adjusted Rand index.
#'
#' @export
#'
calc_ari <- function(...) UseMethod("calc_ari")



#' @describeIn calc_ari
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_ari.table <- function(tbl) {

  sumnij <- sum(choose(tbl, 2))
  sumai <- sum(choose(colSums(tbl), 2))
  sumbj <- sum(choose(rowSums(tbl), 2))
  Ntwo <- choose(sum(tbl), 2)

  abs(
    (sumnij - (sumai*sumbj) / Ntwo) /
      (0.5*(sumai + sumbj) - (sumai*sumbj) / Ntwo)
  )

}



#' @describeIn calc_ari
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_ari.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_ari(tbl)

}

