#' Calculate average AUC (aunu) / AUC macro.
#'
#' @export
#'
calc_aunu <- function(...) UseMethod("calc_aunu")



#' @describeIn calc_aunu
#'
#' @param data data with prediction, reference
#' @param prediction Name of the variable in data that holds the predictions
#' @param reference Name of the variable in data that holds the reference values
#'
#' @export
#'
calc_aunu.data.frame <- function(
    data,
    prediction = "prediction",
    reference = "reference"
) {

  data <- data[,c(prediction, reference)]
  tbl <- table(data)

  calc_aunu(tbl)

}



#' @describeIn calc_aunu
#'
#' @param tbl table
#'
#' @export
#'
calc_aunu.table <- function(tbl) {

  aucs <- double(length = ncol(tbl))

  for (t in 1:ncol(tbl)) {

    tbl.t <- aggregate_multiclass_cm.table(tbl, t)
    tp <- tbl.t[2,2]
    tn <- tbl.t[1,1]
    fp <- tbl.t[2,1]
    fn <- tbl.t[1,2]
    aucs[t] <- calc_auroc(tn, fp, tp, fn)

  }

  calc_macro(aucs)

}





