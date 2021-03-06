% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/laus.R
\name{laus_download}
\alias{laus_download}
\title{Download data from the Local Area Unemployment Statistics (LAUS) database}
\usage{
laus_download(
  bls_key,
  start_year,
  end_year,
  adjustment,
  states,
  data_types,
  clean = TRUE
)
}
\arguments{
\item{bls_key}{BLS API key for the user. See vignette.}

\item{start_year}{Year to start data download.}

\item{end_year}{Year to end data download.}

\item{adjustment}{Character vector. Seasonal adjustment ("S") or not ("U") or both.}

\item{states}{Character vector. See vignette and \code{laus_codes_list} object.}

\item{data_types}{Character vector. See vignette and \code{laus_codes_list} object.}

\item{clean}{Optional logical. Whether to clean the data into usable format or not.
Defaults to TRUE.}
}
\value{
Cleaned data frame of LAUS data.
}
\description{
\code{laus_download} retrieves and downloads data from the Local Area Unemployment
Statistics (LAUS) database.
}
\details{
This function constructs BLS series IDs for a chosen set of parameters and then downloads
the data from the LAUS database. This function also cleans the data and appends on identifiers
for easy panel formatting.
}
\examples{
laus_download(bls_key = "inputs", start_year = 2010, end_year = 2015,
adjustment = "U", states = c("ST0100000000000", "ST0200000000000"), data_types = c("03"))
}
