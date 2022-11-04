% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiclass-calc-cramer-v.R
\name{calc_cramer_v}
\alias{calc_cramer_v}
\title{Calculate Cramer's V.}
\usage{
calc_cramer_v(
  tbl,
  ci.type = c("ncchisq", "ncchisqadj", "fisher", "fisheradj"),
  ci.level = 0.95,
  bias_correct = FALSE
)
}
\arguments{
\item{tbl}{The contingency table.}

\item{ci.type}{FALSE if no ci is requested or one out of "ncchisq" (using noncentral chisquare), "ncchisqadj", "fisher" (using fisher z transformation), "fisheradj" (using fisher z transformation and bias correction).}

\item{ci.level}{double between 0 and 1}

\item{bias_correct}{should a bias correction be applied?}
}
\value{
A numeric vector with the three elements 'v', 'll' and 'ul'. If no CI is requested, 'll' and 'ul' are NA.
}
\description{
Calculate Cramer's V.
}
\details{
A Cramer's V between 0 and 0.3 is considered as weak, 0.3-0.7 as medium and > 0.7 as strong.
This implementation is based on code by Andri Signorell \href{mailto:andri@signorell.net}{andri@signorell.net} and Michael Smithson \href{mailto:michael.smithson@anu.edu.au}{michael.smithson@anu.edu.au} for the package DescTools.
}
\references{
Cramer, H. (1946) Mathematical Methods of Statistics. Princeton University Press
Agresti, A. (2002) Categorical Data Analysis. John Wiley & Sons
}