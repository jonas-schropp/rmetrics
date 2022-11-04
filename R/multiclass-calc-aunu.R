
#' Calculate average AUC (aunu) / AUC macro.
#'
#' @param data data with prediction, reference
#' @param classes confusion matrix classes
#'
#' @export
#'
calc_aunu <- function(data, classes) {

  aucs <- double(length(classes))

  for (t in 1:length(classes)) {

    df_tmp <- aggregate_multiclass_cm(data, classes[t])
    tbl <- table(df_tmp)
    tp <- tbl[2,2]
    tn <- tbl[1,1]
    fp <- tbl[2,1]
    fn <- tbl[1,2]
    aucs[t] <- calc_auroc(tn, fp, tp, fn)

  }

  calc_macro(aucs)

}
