% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jolts.R
\name{jolts_others}
\alias{jolts_others}
\title{Download other separations data from the Job Opening and Labor Turnover survey (JOLTS).}
\usage{
jolts_others(
  bls_key,
  series = "nfp",
  start_year,
  end_year,
  adjustment,
  data_levels = "R"
)
}
\arguments{
\item{bls_key}{BLS API key for the user. See vignette.}

\item{series}{Character. Data series. Either non-farm other ("nfp"), private other
("private"), supersector other ("super"). or sector other ("sector"). Defaults to non-farm other.}

\item{start_year}{Numeric. Year to start data download.}

\item{end_year}{Numeric. Year to end data download.}

\item{adjustment}{Character vector. Seasonal adjustment ("S") or not ("U") or both.}

\item{data_levels}{String. Rate ("R") or level ("L"). Defaults to rate as reported by JOLTS.}
}
\description{
\code{jolts_seps} downloads pre-packaged other data from the JOLTS database.
}
\details{
This function downloads and cleans pre-packaged other separations data from the JOLTS database.
The user has three choices for other separations data: non-farm, private, or super
sector. The data is formatted in terms of rates by default to be similar to the quoted JOLTS
series. The function reflects the October 2020 update to the JOLTS data series.
}
\examples{
jolts_df = jolts_others(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")

}
