% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary-calc-lift.R
\name{calc_lift}
\alias{calc_lift}
\alias{calc_lift.default}
\alias{calc_lift.table}
\alias{calc_lift.data.frame}
\title{Calculate Lift Score}
\usage{
calc_lift(...)

\method{calc_lift}{default}(tp, fp, pos, neg, ...)

\method{calc_lift}{table}(tbl, incr = FALSE, ...)

\method{calc_lift}{data.frame}(data, prediction, reference, incr = FALSE, ...)
}
\arguments{
\item{...}{Additional arguments. Not used.}

\item{tp}{Numeric, True Positives (TP).}

\item{fp}{Numeric, False Positives (FP).}

\item{pos}{Number of positives in reference.}

\item{neg}{Number of negatives in reference.}

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
Calculate Lift Score
}
\section{Methods (by class)}{
\itemize{
\item \code{calc_lift(default)}: 

\item \code{calc_lift(table)}: 

\item \code{calc_lift(data.frame)}: 

}}
