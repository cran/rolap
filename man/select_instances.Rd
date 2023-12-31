% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flat_table_transform.R
\name{select_instances}
\alias{select_instances}
\alias{select_instances.flat_table}
\title{Select instances of a flat table by value}
\usage{
select_instances(ft, not, attributes, values)

\method{select_instances}{flat_table}(ft, not = FALSE, attributes = NULL, values)
}
\arguments{
\item{ft}{A \code{flat_table} object.}

\item{not}{A boolean.}

\item{attributes}{A vector of names.}

\item{values}{A list of value vectors.}
}
\value{
A \code{flat_table} object.
}
\description{
Select only the indicated instances from the flat table.
}
\details{
Several values can be indicated for attributes (performs an OR operation)
or several attributes and a value for each one (performs an AND operation).

If the parameter \code{not} is true, the reported values are those that are not
included.
}
\examples{

ft <- flat_table('iris', iris) |>
  select_instances(attributes = c('Species'),
                   values = c('versicolor', 'virginica'))

ft <- flat_table('ft_num', ft_num) |>
  select_instances(
    not = TRUE,
    attributes = c('Year', 'WEEK'),
    values = list(c('1962', '2'), c('1964', '2'))
  )

}
\seealso{
\code{\link{flat_table}}

Other flat table transformation functions: 
\code{\link{add_custom_column}()},
\code{\link{remove_instances_without_measures}()},
\code{\link{replace_empty_values}()},
\code{\link{replace_string}()},
\code{\link{replace_unknown_values}()},
\code{\link{select_attributes}()},
\code{\link{select_instances_by_comparison}()},
\code{\link{select_measures}()},
\code{\link{separate_measures}()},
\code{\link{transform_attribute_format}()},
\code{\link{transform_from_values}()},
\code{\link{transform_to_attribute}()},
\code{\link{transform_to_measure}()},
\code{\link{transform_to_values}()}
}
\concept{flat table transformation functions}
