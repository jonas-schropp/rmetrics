#' Calculate Relative classifier information (rci).
#'
#' @export
#'
calc_rci <- function(...) UseMethod("calc_rci")



#' @describeIn calc_rci
#'
#' @param data data
#' @param prediction prediciton
#' @param reference reference
#'
#' @export
#'
calc_rci.data.frame <- function(
    data,
    prediction = "prediction",
    reference = "reference"
) {

  tbl <- table(data[, c(prediction, reference)])

  calc_rci(tbl)

}



#' @describeIn calc_rci
#'
#' @param tbl table
#'
#' @export
#'
calc_rci.table <- function(tbl) {

  n <- sum(tbl)
  pos <- colSums(tbl)

  mutual_information <- calc_mutual_information(tbl)
  reference_entropy <- calc_reference_entropy(pos, n)

  if (reference_entropy != 0) {
    mutual_information / reference_entropy
  } else {
    NA
  }

}
