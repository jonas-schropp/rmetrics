#' Calculate Mean overall Jaccard index.
#'
#' @param ... `r rox("dots")`
#'
#' The overall Jaccard index is calculated as the mean Jaccard index over all classes in data.
#'
#' @export
#'
calc_jaccard_overall <- function(...) UseMethod("calc_jaccard_overall")



#' @describeIn calc_jaccard_overall
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#'
#' @export
calc_jaccard_overall.default <- function(tp, fn, fp, ...) {

  res <- calc_jaccard(tp, fn, fp)

  mean(res)

}



#' @describeIn calc_jaccard_overall
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
calc_jaccard_overall.table <- function(tbl, ...) {

  res <- double(length = ncol(tbl))

  for (t in 1:ncol(tbl)) {

    tbl.t <- aggregate_multiclass_cm.table(tbl, t)
    tp <- tbl.t[2,2]
    fp <- tbl.t[2,1]
    fn <- tbl.t[1,2]

    res[t] <- calc_jaccard(tp, fn, fp)

  }

  mean(res)

}



#' @describeIn calc_jaccard_overall
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
calc_jaccard_overall.data.frame <- function(
    data,
    prediction = "prediction",
    reference = "reference", ...
    ) {

  data <- data[,c(prediction, reference)]
  tbl <- table(data)

  calc_jaccard_overall(tbl)

}
