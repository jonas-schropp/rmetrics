#' Roxygen Arguments to Reuse.
#'
#'
#' @noRd
#' @keywords Internal
#'
rox <- function(type) {

  if (type == "tbl") {
    "A table representing the input confusion matrix. This must always have prediction on rows and reference on columns, otherwise most functions in rmetrics will generate incorrect results."
  } else if (type == "data") {
    "A data.frame containing the prediction and the reference."
  } else if (type == "prediction") {
    "Character. The name of the variable in data that contains the predictions."
  } else if (type == "reference") {
    "Character. The name of the variable in data that contains the reference values."
  } else if (type == "n") "Total number of observations."

  else if (type == "tp") "Numeric, True Positives (TP)."
  else if (type == "fp") "Numeric, False Positives (FP)."
  else if (type == "tn") "Numeric, True Negatives (TN)."
  else if (type == "fn") "Numeric, Fase Negatives (FN)."

  else if (type == "tpm") "Numeric vector of True Positives (TP) by class."
  else if (type == "fpm") "Numeric vector of False Positives (FP) by class."
  else if (type == "tnm") "Numeric vector of True Negatives (TN) by class."
  else if (type == "fnm") "Numeric vector of Fase Negatives (FN) by class."

  else if (type == "otp") "Overall True Positives (OTP)."
  else if (type == "ofp") "Overall False Positives (OFP)."
  else if (type == "otn") "Overall True Negatives (OTN)."
  else if (type == "ofn") "Overall Fase Negatives (OFN)."

  else if (type == "ppos") "Number of positives in prediction vector (= TP + FP)"
  else if (type == "pos") "Number of positives in reference."
  else if (type == "neg") "Number of negatives in reference."

  else if (type == "posm") "Number of positives per class (= colSums(tbl))"
  else if (type == "negm") "Number of negatives per class (= colSums(tbl))"

  else if (type == "ci.level") {
    "A number between 0 and 1 for the levels of the confidence intervals that should be calculated."
  }
  else if (type == "method") {
    'Character. The averaging method for the individual class scores. Can be either "macro" for macro-averaging or "micro" for micro averaging.'
  }
  else if (type == "dots") "Additional arguments. Not used."

}
