% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary-calc-err.R
\name{calc_err}
\alias{calc_err}
\alias{calc_err.default}
\alias{calc_err.table}
\alias{calc_err.data.frame}
\title{Calculate Error Rate.}
\usage{
calc_err(...)

\method{calc_err}{default}(tp, tn, fp, fn, ci.type, ci.level, ...)

\method{calc_err}{table}(tbl, ci.type, ci.level, incr = FALSE, ...)

\method{calc_err}{data.frame}(data, prediction, reference, ci.type, ci.level, incr = FALSE, ...)
}
\arguments{
\item{...}{Additional arguments. Not used.}

\item{tp}{Numeric, True Positives (TP).}

\item{tn}{Numeric, True Negatives (TN).}

\item{fp}{Numeric, False Positives (FP).}

\item{fn}{Numeric, Fase Negatives (FN).}

\item{ci.type}{Either FALSE if no confidence intervals are desired or one of
"agresti.coull", "agresti-coull", "ac", "asymptotic", "normal",
"wald", "clopper-pearson", "cp", "exact", "jeffreys", "bayes",
and "wilson". If FALSE, overwrites ci.level.}

\item{ci.level}{A number between 0 and 1 for the levels of the confidence intervals that
should be calculated.}

\item{tbl}{A table representing the input confusion matrix. This must always have
prediction on rows and reference on columns, otherwise most functions in
rmetrics will generate incorrect results.}

\item{incr}{Double. Continuity correction to add to each cell of the table. FALSE
by default, which means the raw counts will be used.}

\item{data}{A data.frame containing the prediction and the reference.}

\item{prediction}{Character. The name of the variable in data that contains the predictions.}

\item{reference}{Character. The name of the variable in data that contains the reference
values.}
}
\description{
The error rate of a classification model is a measure of the proportion
of incorrect predictions made by the model. It is calculated as the number
of incorrect predictions divided by the total number of predictions made by
the model.
}
\details{
To calculate the error rate of a classification model, the following formula is used:

Error rate = (Number of incorrect predictions) / (Total number of predictions)

The resulting value is a proportion, with values closer to 0 indicating a
better-performing model and values closer to 1 indicating a worse-
performing model.
}
\section{Methods (by class)}{
\itemize{
\item \code{calc_err(default)}: 

\item \code{calc_err(table)}: 

\item \code{calc_err(data.frame)}: 

}}
