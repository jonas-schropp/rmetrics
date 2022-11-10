#' Calculate average AUC (aunu) / AUC macro.
#'
#' @export
#'
calc_aunu <- function(...) UseMethod("calc_aunu")



#' @describeIn calc_aunu
#'
#' @param tp `r rox("tpm")`
#' @param tn `r rox("tnm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#'
#' @export
#'
calc_aunu.default <- function(tp, tn, fp, fn) {

  aucs <- double(length = length(tp))
  for (i in 1:length(tp)) aucs[i] <- calc_auroc(tn[i], fp[i], tp[i], fn[i])

  calc_macro(aucs)

}



#' @describeIn calc_aunu
#'
#' @param tbl `r rox("tbl")`
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



#' @describeIn calc_aunu
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
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





