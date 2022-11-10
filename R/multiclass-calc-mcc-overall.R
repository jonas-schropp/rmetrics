#' Calculate Overall Matthews Correlation Coefficient.
#'
#' @export
#'
calc_mcc_overall <- function(...) UseMethod("calc_mcc_overall")



#' @describeIn calc_mcc_overall
#'
#' @param tp `r rox("tpm")`
#' @param fp `r rox("fpm")`
#' @param fn `r rox("fnm")`
#'
#' @export
#'
calc_mcc_overall.default <- function(tp, fp, fn) {

  ppos <- tp + fp
  pos <- tp + fn

  cov_x_y <- sum(tp * sum(ppos) - pos * ppos)
  cov_x_x <- sum(ppos * (sum(ppos) - ppos))
  cov_y_y <- sum(pos * (sum(ppos) - pos))

  cov_x_y / (sqrt(cov_y_y * cov_x_x))

}



#' @describeIn calc_mcc_overall
#'
#' @param tbl `r rox("tbl")`
#'
#' @export
#'
calc_mcc_overall.table <- function(tbl) {

  tp <- diag(tbl)
  fn <- colSums(tbl) - tp
  fp <- rowSums(tbl) - tp
  tn <- sum(tbl) - tp - fn - fp

  calc_mcc_overall(tp, fp, fn)

}



#' @describeIn calc_mcc_overall
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#'
#' @export
#'
calc_mcc_overall.data.frame <- function(
    data,
    prediction, reference
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_mcc_overall(tbl)

}

