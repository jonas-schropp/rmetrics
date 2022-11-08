#' Calculate Mean overall Jaccard index.
#'
#'
#' @export
#'
calc_jaccard_overall <- function(...) UseMethod("calc_jaccard_overall")



#' @describeIn calc_jaccard_overall
#'
#' @param data data frame or matrix consisting of prediction and reference
#' @param prediction Name of the variable in data that holds the predictions
#' @param reference Name of the variable in data that holds the reference values
#'
#' @export
calc_jaccard_overall.data.frame <- function(
    data,
    prediction = "prediction",
    reference = "reference"
    ) {

  data <- data[,c(prediction, reference)]
  tbl <- table(data)

  calc_jaccard_overall(tbl)

}



#' @describeIn calc_jaccard_overall
#'
#' @param tbl confusion matrix
#'
#' @export
calc_jaccard_overall.table <- function(tbl) {

  res <- double(length = ncol(tbl))

  for (t in 1:ncol(tbl)) {

    tbl.t <- aggregate_multiclass_cm.table(tbl, t)
    tp <- tbl.t[2,2]
    fp <- tbl.t[2,1]
    pos <- sum(tbl.t[, 2])
    ppos <- tp + fp
    res[t] <- calc_jaccard(tp, ppos, pos)

  }

  mean(res)

}
