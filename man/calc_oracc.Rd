% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiclass-calc-oracc.R
\name{calc_oracc}
\alias{calc_oracc}
\alias{calc_oracc.default}
\alias{calc_oracc.table}
\alias{calc_oracc.data.frame}
\title{Calculate Calculate (Unbiased) Overall Random Accuracy.}
\usage{
calc_oracc(...)

\method{calc_oracc}{default}(tp, fp, fn, n, unbiased = FALSE, ...)

\method{calc_oracc}{table}(tbl, unbiased = FALSE, ...)

\method{calc_oracc}{data.frame}(data, prediction, reference, unbiased = FALSE, ...)
}
\arguments{
\item{...}{Additional arguments. Not used.}

\item{tp}{Numeric vector of True Positives (TP) by class.}

\item{fp}{Numeric vector of False Positives (FP) by class.}

\item{fn}{Numeric vector of Fase Negatives (FN) by class.}

\item{n}{Total number of observations.}

\item{unbiased}{TRUE/FALSE, should unbiased random accuracy be returned? FALSE by default.}

\item{tbl}{A table representing the input confusion matrix. This must always have
prediction on rows and reference on columns, otherwise most functions in
rmetrics will generate incorrect results.}

\item{data}{A data.frame containing the prediction and the reference.}

\item{prediction}{Character. The name of the variable in data that contains the predictions.}

\item{reference}{Character. The name of the variable in data that contains the reference
values.}
}
\description{
Calculate Calculate (Unbiased) Overall Random Accuracy.
}
\section{Methods (by class)}{
\itemize{
\item \code{calc_oracc(default)}: 

\item \code{calc_oracc(table)}: 

\item \code{calc_oracc(data.frame)}: 

}}
