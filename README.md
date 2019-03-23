
<!-- README.md is generated from README.Rmd. Please edit that file -->
blsr
====

An R package for obtaining and cleaning data from the Bureau of Labor Statistics (BLS). This product uses the BLS API and the blscrapeR package.

Highlights
----------

-   Simple-to-use functions for downloading data from the most commonly used BLS databases.

-   Cleans the data returned by the BLS API and converts the data into a panel format.

-   Converts user inputs into BLS series ID strings and implements error-checks.

Usage
-----

A more detailed usage description can be found in the [vignette](www.davidsovich.com).

Current, the supported BLS databases include:

-   Current employment statistics (CES)

-   Job openings and labor turnover survey (JOLTS)

-   Local area unemployment statistics (LAUS)

Examples:

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
