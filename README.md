
<!-- README.md is generated from README.Rmd. Please edit that file -->
blsr
====

An R package for obtaining and cleaning data from the Bureau of Labor Statistics (BLS). This product uses the BLS API and the blscrapeR package.

Highlights
----------

-   Simple-to-use functions for downloading data from the most commonly used BLS databases.

-   Returns cleaned data frames ready for panel analysis.

Usage
-----

A more detailed usage description can be found in the [vignette](www.davidsovich.com).

``` r
library(blscrapeR)
#Download data from CES

#Download data from JOLTS
```

Installation
------------

The blsr package is not available on CRAN. You can install the development version from Github:

``` r
library("devtools")
devtools::install_github("davidsovich/blsr")
```

History
-------

-   March 22, 2019: Developmental release
