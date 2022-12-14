% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiclass-calc-rci.R
\name{calc_rci}
\alias{calc_rci}
\alias{calc_rci.table}
\alias{calc_rci.data.frame}
\title{Calculate Relative Classifier Information.}
\usage{
calc_rci(...)

\method{calc_rci}{table}(tbl, ...)

\method{calc_rci}{data.frame}(data, prediction = "prediction", reference = "reference", ...)
}
\arguments{
\item{...}{Additional arguments. Not used.}

\item{tbl}{A table representing the input confusion matrix. This must always have
prediction on rows and reference on columns, otherwise most functions in
rmetrics will generate incorrect results.}

\item{data}{A data.frame containing the prediction and the reference.}

\item{prediction}{Character. The name of the variable in data that contains the predictions.}

\item{reference}{Character. The name of the variable in data that contains the reference
values.}
}
\description{
Calculate Relative Classifier Information.
}
\section{Methods (by class)}{
\itemize{
\item \code{calc_rci(table)}: 

\item \code{calc_rci(data.frame)}: 

}}
