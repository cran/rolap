% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoattribute.R
\name{get_layer_from_attribute}
\alias{get_layer_from_attribute}
\title{Get layer from attribute}
\usage{
get_layer_from_attribute(
  db,
  dimension = NULL,
  attribute = NULL,
  from_attribute = NULL
)
}
\arguments{
\item{db}{A \code{star_database} object.}

\item{dimension}{A string, dimension name.}

\item{attribute}{A string, attribute name.}

\item{from_attribute}{A string, attribute name.}
}
\value{
A \code{star_database} object.
}
\description{
Gets the geographic layer associated with the from_attribute at the level of
the indicated attributes.
}
\keyword{internal}
