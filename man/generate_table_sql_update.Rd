% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refresh.R
\name{generate_table_sql_update}
\alias{generate_table_sql_update}
\title{Generate table sql update}
\usage{
generate_table_sql_update(table, surrogate_keys, instances)
}
\arguments{
\item{table}{A string, table name.}

\item{surrogate_keys}{A string.}

\item{instances}{A \code{tibble}.}
}
\value{
A vector of strings.
}
\description{
Generate sql code for updating a table.
}
\keyword{internal}
