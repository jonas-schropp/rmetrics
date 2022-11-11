# Introduction

`rmetrics` is a R package to calculate a large variety of metrics to
evaluate machine learning models and statistical models. It is primarily
designed as a backend to my package `revaluate`, but the large number of
provided metrics makes it useful as a standalone package.

The package provides classsification metrics for *binary*, *multiclass*
or *ordered* dependent variables, with the special case of binary
dependent variables that are evaluated using upper and lower cutoffs and
a grey area in between.

All functions in the package provide several methods based on the class
of the first input variable. In most cases, the functions work with raw
counts from the confusion matrix (TP, FP, TN, FN etc), a `table` object
or a `data.frame`.

The package is **under active development**! Function names or arguments
might change unannounced, methods might be added or even removed. Do not
use it in a production environment!

# Installation

`rmetrics` is not yet on CRAN. You can install the package via github:

    devtools::install_github("jonas-schropp/rmetrics.git")

And load it with:

    library(rmetrics)

# Usage

Let’s calculate Aickin’s alpha as an example:

    # Simulate some data
    pred <- c(rep(1, 50), rep(2, 50), rep(3, 50))
    ref <- c(rep(1, 25), rep(2, 5), rep(3, 20),
                   rep(2, 40), rep(1, 5), rep(3, 5),
                   rep(3, 30), rep(1, 10), rep(2, 10))

    tbl <- table(pred, ref)

    calc_aickin(tbl = tbl)

    ##     alpha        ll        ul 
    ## 0.4509621 0.3358289 0.5660953

We could also use a data.frame object as input and set the CI to 99%:

    calc_aickin(
      data.frame(pred, ref),
      prediction = "pred",
      reference = "ref",
      ci.level = 0.99
      )

    ##     alpha        ll        ul 
    ## 0.4509621 0.2996514 0.6022728

Or not request a CI at all:

    calc_aickin(tbl, ci.type = FALSE)

    ##     alpha        ll        ul 
    ## 0.4509621        NA        NA

# Available metrics

For a full list of functions, see the reference manual.

# Contribute to this project

If you want to contribute to this project, please get in touch!

If there is a metric you want to see in this package that is not
available, please either submit a `feature request` or implement it
yourself and submit a `pull request`.

# Planned features

This package is under active development. The current focus for
development is 1) to implement more metrics for ordinal classifiers and
2) to add confidence interval calculations for more of the metrics.
