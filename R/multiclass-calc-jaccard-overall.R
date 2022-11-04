
#' Calculate Mean overall Jaccard index.
#'
#' @param data data frame or matrix consisting of prediction and reference
#'
#' @export
#'
calc_jaccard_overall <- function(data) {

  classes <- levels(data[[1]])
  res <- double(length(classes))

  for (t in 1:length(classes)) {

    df_tmp <- aggregate_multiclass_cm(data, classes[t])
    tbl <- table(df_tmp)
    tp = tbl[2,2]
    fp = tbl[2,1]
    pos <- sum(tbl[,2])
    ppos <- tp + fp
    res[t] <- calc_ji(tp, ppos, pos)

  }

  mean(res)

}
