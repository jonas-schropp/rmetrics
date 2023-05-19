#' Calculate Adjusted F-score (af).
#'
#' This function calculates the Adjusted F-score (af) which is a statistical
#' measure that combines both sensitivity and specificity of a binary
#' classification model, by taking into account both the false positive rate
#' (FPR) and the false negative rate (FNR) while penalizing more heavily the
#' FNR. This function uses the calc_f function to calculate the F2-score (F2)
#' and the inverse of the F0.5-score (inv05) and then applies the following
#' formula: sqrt(F2 * inv05).
#'
#' @param ... `r rox("dots")`
#'
#' @return A numeric value that represents the Adjusted F-score (af) for the
#' binary classification model.
#'
#' @export
#'
#' @examples
#' calc_af(tp = 80, fp = 20, fn = 20, tn = 80)
#'
#' @seealso
#' \code{\link{calc_f}}
#'
#' @keywords classification
#'
#' @family Classification Metrics
#'
#' @describeIn calc_af
#'
#' @return A numeric value that represents the Adjusted F-score (af) for the
#' binary classification model.
#'
calc_af <- function(...) {

  UseMethod("calc_af")

}



#' @describeIn calc_af
#'
#' @param tp `r rox("tp")`
#' @param fp `r rox("fp")`
#' @param fn `r rox("fn")`
#' @param tn `r rox("tn")`
#'
#' @export
#'
calc_af.default <- function(tp, fp, fn, tn, ...) {

  f2 <- calc_f(tp = tp, fp = fp, fn = fn, beta = 2)
  inv05 <- calc_f(tp = tn, fp = fn, fn = fp, beta = 0.5)

  sqrt(f2 * inv05)

}



#' @describeIn calc_af
#'
#' @param tbl `r rox("tbl")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_af.table <- function(
    tbl,
    incr = FALSE,
    ...
    ) {

  tbl <- tbl + incr

  calc_af(
    tp = tbl[2,2],
    fp = tbl[2,1],
    fn = tbl[1,2],
    tn = tbl[1,1]
    )

}



#' @describeIn calc_af
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param incr `r rox("incr")`
#'
#' @export
#'
calc_af.data.frame <- function(
    data,
    prediction,
    reference,
    incr = FALSE,
    ...
) {

  tbl <- table(data[, c(prediction, reference)])

  calc_af(tbl, incr)

}
