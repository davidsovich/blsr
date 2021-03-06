% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_bls_metadata}
\alias{get_bls_metadata}
\title{Displays metadata from the Bureau of Labor Statistics}
\usage{
get_bls_metadata(database)
}
\arguments{
\item{database}{Character. Database name. Either "JOLTS", "CES", or "CESstate".}
}
\value{
Data frame containing BLS metadata.
}
\description{
\code{get_bls_metadata} displays metatadata for a given Bureau of Labor Statistics (BLS)
database.
}
\details{
This function displays metadata for a BLS database.
}
\examples{
get_bls_metadata("JOLTS")
}
