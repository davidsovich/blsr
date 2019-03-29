
<!-- README.md is generated from README.Rmd. Please edit that file -->
blsr
====

An R package for obtaining and cleaning data from the Bureau of Labor Statistics (BLS). This product uses the BLS API and requires that the user obtain an API key from the BLS.

Highlights
----------

-   Simple-to-use functions for downloading data from the most commonly used BLS databases.

-   Cleans the data returned by the BLS API and converts the data into a panel format.

-   Can be used to supplement the blscrapeR package by converting user inputs into BLS series ID strings and implementing error-checks.

Usage
-----

This package retrieves data from the economic databases provided by the BLS. A more detailed usage description can be found in the [vignette](www.davidsovich.com).

This package currently supports the following BLS databases:

-   Current employment statistics (CES)

-   Job openings and labor turnover survey (JOLTS)

-   Local area unemployment statistics (LAUS)

In the near future, this package should also support these additional BLS databases:

-   Quarterly Census of Employment and Wages (QCEW)

-   Current Population Survey (CPS)

-   Consumer Price Index (CPI)

-   Current Expenditure Survey (CE)

Examples:

``` r
library(blscrapeR)


#Download data from CES

   # Custom data series for CES state data
   ces_df = ces_download(bls_key = Sys.getenv("BLS_KEY"),
                      start_year = 2010,
                      end_year = 2015,
                      adjustment = "U",
                      industries = "05000000",
                      data_types = c("01", "03", "11"),
                      states = "1900000")
   
   # Pre-build CES data series for national employment
   ces_df = ces_employment(bls_key = Sys.getenv("BLS_KEY"),
                           start_year = 2010,
                           end_year = 2015)

#Download data from JOLTS

   # Custom data series for JOLTS regional data
   jolts_df = jolts_download(bls_key = Sys.getenv("BLS_KEY"),
                             start_year = 2010,
                             end_year = 2015,
                             adjustment = "S",
                             industries = "000000",
                             data_types = "HI",
                             data_levels = "L",
                             regions = c("MW", "NE", "SO", "WE"))
   
   # Pre-built JOLTS data series
   jolts_df = jolts_hiring(bls_key = Sys.getenv("BLS_KEY"),
                           start_year = 2010,
                           end_year = 2015)


# Local area unemployment statistics

   # Custom data series
   laus_df = laus_download(bls_key = Sys.getenv("BLS_KEY"),
                           start_year = 2010,
                           end_year = 2015,
                           adjustment = "S", 
                           states = c("ST0100000000000", "ST0200000000000"), 
                           data_types = c("03"))
   
   # Pre-built data series
   laus_df = laus_urates(bls_key = Sys.getenv("BLS_KEY"),
                         start_year = 2010,
                         end_year = 2015)
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
