# Introduction

`rmetrics` is a R package to calculate a large variety of metrics to
evaluate machine learning models and statistical models. It is primarily
designed as a backend to my package `revaluate`, but will receive some
work to make it worthwhile as a standalone package in the future.

Metrics in the package can generally be classified into metrics for a
*binary*, *multiclass* or *ordered* dependent variable, with the special
case of binary dependent variables that are evaluated using upper and
lower cutoffs and a grey area in between.

# Installation

You can install the package via github:

    devtools::install_github("https://github.com/jonas-schropp/rmetrics.git")

And load it with:

    library(rmetrics)

# Usage

Let’s calculate Aickin’s alpha as an example:

    # Simulate some data
    prediction <- rbinom(100, 1, 0.5)
    reference <- rbinom(100, 1, prediction * 0.7)

    tbl <- table(prediction, reference)

    calc_aickin(tbl = tbl)

    ## [1] 0.8744661 0.7999319 0.9490003

# Contribute to this project

If you want to contribute to this project, please get in touch!

If there is a metric you want to see in this package that is not
available, please either submit a `feature request` or implement it
yourself and submit a `pull request`.

# Planned features

This package is under active development. The current focus for
development is 1) to implement more metrics for ordinal classifiers and
2) to make sure that all functions follow common design features.
