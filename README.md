
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pitagoras

<!-- badges: start -->

[![R-CMD-check](https://github.com/thalesdrago/pitagoras/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thalesdrago/pitagoras/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of pitagoras is to be a dummy package that I use to understand
R package building It uses as example a single function called
my_shannon(), that calculates Shannon diversity

## Installation

You can install the development version of pitagoras like so:

``` r
install_github("thalesdrago/pitagoras")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pitagoras)
## Basic example code

# Community vector
x <- c(1, 4, 5, 2, 1, 6)

pitagoras::my_shannon(x)
#> [1] 1.59027
```
