% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{clean_ces_national}
\alias{clean_ces_national}
\title{Cleans the results of a query to the CES database.}
\usage{
clean_ces_national(bls_df)
}
\arguments{
\item{bls_df}{Data frame containing results of query to the CES national database.}
}
\value{
Cleaned data frame from CES national database.
}
\description{
\code{clean_ces_national} cleans the results of a query to the CES national database and appends
additional information.
}
\details{
General data cleaning.
}
\examples{
clean_ces_national(ces_df)
}
