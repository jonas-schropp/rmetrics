#' Calculate Rand Index.
#'
#' The Rand index ranges between 0 and 1 and can be used to measure the similarity between two categorical vectors. It is commonly used to evaluate the similarity between clustering outcomes.
#'
#' @references
#' Rand, W.M. 1971. Objective criteria for the evaluation of clustering methods. Journal of the American Statistical Association 66: 846–850.
#' Hubert, L. and Arabie, P. 1985. Comparing partitions. Journal of Classification. 2: 193–218.
#'
#' @author
#' Adapted from the implementation for Rand Index and Adjusted Rand index in `fossil`, written by Matthew Vavrek.
#'
#' @export
#'
calc_rand <- function(...) UseMethod("calc_rand")



#' @describeIn calc_rand
#'
#' @param tbl `r rox("tbl")`
#' @param adjust Should the index be rescaled to take into account that random chance will cause some objects to occupy the same clusters, so that the Rand Index can never be zero? FALSE by default.
#'
#' @export
#'
calc_rand.table <- function(tbl, adjust = FALSE) {

  if(adjust) {

    sumnij <- sum(choose(tbl, 2))
    sumai <- sum(choose(colSums(tbl), 2))
    sumbj <- sum(choose(rowSums(tbl), 2))
    Ntwo <- choose(sum(tbl), 2)

    return(
      abs(
        (sumnij - (sumai*sumbj) / Ntwo) /
          (0.5*(sumai + sumbj) - (sumai*sumbj) / Ntwo)
      )
    )

  } else {
    df <- as.data.frame(tbl)
    p <- NULL
    r <- NULL
    for (i in 1:nrow(df)) {
      p <- c(p, rep(df[[1]][i], each = df[[3]][i]))
      r <- c(r, rep(df[[2]][i], each = df[[3]][i]))
    }
    data <- data.frame(prediction = p, reference = r)

    calc_rand.data.frame(data,
                         prediction = "prediction",
                         reference = "reference",
                         adjust = FALSE)

  }


}



#' @describeIn calc_rand
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param adjust Should the index be rescaled to take into account that random chance will cause some objects to occupy the same clusters, so that the Rand Index can never be zero? FALSE by default.
#'
#' @export
#'
calc_rand.data.frame <- function(
    data,
    prediction = "prediction",
    reference = "reference",
    adjust = FALSE
    ) {

  data <- data[, c(prediction, reference)]
  prediction <- data[[1]]
  reference <- data[[2]]

  if (adjust) {

    calc_rand.table(tbl, adjust = TRUE)

  } else {

    x <- vapply(prediction, FUN = function(x) x != prediction, integer(length(prediction)))
    y <- vapply(reference, FUN = function(x) x != reference, integer(length(reference)))

    sg <- sum(abs(x - y)) / 2
    bc <- choose(dim(x)[1], 2)

    return(1 - sg / bc)

  }

}
