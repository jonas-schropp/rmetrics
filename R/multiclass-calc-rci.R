#' Calculate Relative Classifier Information.
#'
#' @export
#'
calc_rci <- function(...) UseMethod("calc_rci")



#' @describeIn calc_rci
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_rci.table <- function(tbl) {

  mutual_information <- calc_mutual_information(tbl)
  reference_entropy <- calc_reference_entropy(tbl)

  if (reference_entropy != 0) {
    return(mutual_information / reference_entropy)
  } else {
    return(NA_real_)
  }

}



#' @describeIn calc_rci
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
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
