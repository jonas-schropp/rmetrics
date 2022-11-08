#' Calculate Kappa, unbiased Kappa or Kappa no Prevalence.
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

  else if (type == "tp") "True Positives (TP)."
  else if (type == "fp") "False Positives (FP)."
  else if (type == "tn") "True Negatives (TN)."
  else if (type == "fn") "Fase Negatives (FN)."

  else if (type == "otp") "Overall True Positives (OTP)."
  else if (type == "ofp") "Overall False Positives (OFP)."
  else if (type == "otn") "Overall True Negatives (OTN)."
  else if (type == "ofn") "Overall Fase Negatives (OFN)."

  else if (type == "ppos") "Number of positives in prediction vector (= TP + FP)"
  else if (type == "pos") "Number of positives per class (= colSums(tbl))"

  else if (type == "ci.level") {
    "A number between 0 and 1 for the levels of the confidence intervals that should be calculated."
  }

}
