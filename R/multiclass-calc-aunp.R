#' Calculate overall AUC (aunp).
#'
#' @export
#'
calc_aunp <- function(...) UseMethod("calc_aunp")



#' @describeIn calc_aunp
#'
#' @param tp `r rox("tpm")`
#' @param tn `r rox("tnm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#'
#' @export
#'
calc_aunp.default <- function(tp, tn, fp, fn) {

  aucs <- double(length = length(tp))
  for (i in 1:length(tp)) aucs[i] <- calc_auroc(tn[i], fp[i], tp[i], fn[i])

  n <- sum(tp, tn, fp, fn)
  pos <- fn + tp

  sum((pos / n) * aucs)

}


#' @describeIn calc_aunp
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_aunp.table <- function(tbl) {

  aucs <- double(length = ncol(tbl))

  for (t in 1:ncol(tbl)) {

    tbl.t <- aggregate_multiclass_cm.table(tbl, t)
    tp <- tbl.t[2,2]
    tn <- tbl.t[1,1]
    fp <- tbl.t[2,1]
    fn <- tbl.t[1,2]
    aucs[t] <- calc_auroc(tn, fp, tp, fn)

  }

  n <- sum(tbl)
  pos <- colSums(tbl)

  sum((pos / n) * aucs)

}



#' @describeIn calc_aunp
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_aunp.data.frame <- function(
    data,
    prediction = "prediction",
    reference = "reference"
) {

  data <- data[,c("prediction", "reference")]
  tbl <- table(data)

  calc_aunp(tbl)

}
