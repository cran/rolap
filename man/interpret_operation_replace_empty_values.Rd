% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_according_to.R
\name{interpret_operation_replace_empty_values}
\alias{interpret_operation_replace_empty_values}
\title{Interpret operation}
\usage{
interpret_operation_replace_empty_values(ft, op, file, last_op)
}
\arguments{
\item{ft}{flat table}

\item{op}{operation}

\item{file}{file to write the code}

\item{last_op}{A boolean, is the last operation?}
}
\value{
A flat table.
}
\description{
operation,             name,       details,                  details2
"replace_empty_values", attributes, empty_values
}
\keyword{internal}
