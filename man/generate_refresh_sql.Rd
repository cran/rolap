% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refresh.R
\name{generate_refresh_sql}
\alias{generate_refresh_sql}
\title{Generate refresh sql}
\usage{
generate_refresh_sql(refresh)
}
\arguments{
\item{refresh}{A list of operations over tables.}
}
\value{
A vector of strings.
}
\description{
Generate sql code for the first refresh operation.
}
\keyword{internal}
