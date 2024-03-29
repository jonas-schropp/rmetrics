% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary-calc-jaccard.R
\name{calc_jaccard}
\alias{calc_jaccard}
\alias{calc_jaccard.default}
\alias{calc_jaccard.table}
\alias{calc_jaccard.data.frame}
\title{Calculate Jaccard index}
\usage{
calc_jaccard(...)

\method{calc_jaccard}{default}(tp, fn, fp, ...)

\method{calc_jaccard}{table}(tbl, incr = FALSE, ...)

\method{calc_jaccard}{data.frame}(data, prediction, reference, incr = FALSE, ...)
}
\arguments{
\item{...}{Additional arguments. Not used.}

\item{tp}{Numeric, True Positives (TP).}

\item{fn}{Numeric, Fase Negatives (FN).}

\item{fp}{Numeric, False Positives (FP).}

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
Calculate Jaccard index
}
\section{Methods (by class)}{
\itemize{
\item \code{calc_jaccard(default)}: 

\item \code{calc_jaccard(table)}: 

\item \code{calc_jaccard(data.frame)}: 

}}
