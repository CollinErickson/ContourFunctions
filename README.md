
<!-- README.md is generated from README.Rmd. Please edit that file -->
cf: Contour Functions
=====================

[![Travis-CI Build Status](https://travis-ci.org/CollinErickson/contour.svg?branch=master)](https://travis-ci.org/CollinErickson/contour)

This is an R package that provides simple functions for creating contour plots.

Overview
--------

The main functions are:

-   `cf_grid`: Makes a contour plot from grid data.

-   `cf_func`: Makes a contour plot for a function.

-   `cf_data`: Makes a contour plot for a data set by fitting a Gaussian process model.

-   `cf`: Passes arguments to `cf_function` or `cf_data` depending on whether the first argument is a function or numeric.

Installation
------------

    # It can be installed like any other package
    install.packages("cf")

    # Or the the development version from GitHub:
    # install.packages("devtools")
    devtools::install_github("CollinErickson/contour")

Usage
-----

``` r

library(cf)
a <- b <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(a^2, b^2, "+"))
cf_grid(a, b, cos(r^2)*exp(-r/(2*pi)))
```

![](tools/README-cf_grid-1.png)

``` r
f1 <- function(r) cos(r[1]^2 + r[2]^2)*exp(-sqrt(r[1]^2 + r[2]^2)/(2*pi))
cf_func(f1, xlim = c(-4*pi, 4*pi), ylim = c(-4*pi, 4*pi))
```

![](tools/README-cf_func-1.png)

``` r
set.seed(0)
x <- runif(20)
y <- runif(20)
z <- exp(-(x-.5)^2-5*(y-.5)^2)
cf_data(x,y,z)
```

![](tools/README-cf_data-1.png)
