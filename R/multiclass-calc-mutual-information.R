#' Calculate Mutual information.
#'
#' @export
#'
calc_mutual_information <- function(...) UseMethod("calc_mutual_information")



#' @describeIn calc_mutual_information
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_mutual_information.table <- function(tbl) {

  ppos <- diag(tbl) + (rowSums(tbl) - diag(tbl))
  n <- sum(tbl)

  re <- calc_response_entropy(ppos, n)
  ce <- calc_conditional_entropy(tbl)

  re - ce
}



#' @describeIn calc_mutual_information
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_mutual_information.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_mutual_information(tbl)

}

