% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic.R, R/refresh.R, R/star_database.R
\name{get_star_database}
\alias{get_star_database}
\alias{get_star_database.star_database_update}
\alias{get_star_database.star_database}
\title{Get star database}
\usage{
get_star_database(db, name)

\method{get_star_database}{star_database_update}(db, name = NULL)

\method{get_star_database}{star_database}(db, name)
}
\arguments{
\item{db}{A \code{star_database_update} object.}

\item{name}{A string, star database name (fact name).}
}
\value{
A \code{star_database} object.
}
\description{
It obtains the star database: For updates, the one defined from the data; for
constellations, the one indicated by the parameter.
}
\examples{

f1 <- flat_table('ft_num', ft_cause_rpd) |>
  as_star_database(mrs_cause_schema_rpd)
f2 <- flat_table('ft_num2', ft_cause_rpd) |>
  update_according_to(f1)
st <- f2 |>
  get_star_database()

db1 <- star_database(mrs_cause_schema, ft_num) |>
  snake_case()
db2 <- star_database(mrs_age_schema, ft_age) |>
  snake_case()
ct <- constellation("MRS", db1, db2)
names <- ct |>
  get_fact_names()
st <- ct |>
  get_star_database(names[1])

}
\seealso{
\code{\link{star_database}}

Other star database refresh functions: 
\code{\link{get_existing_fact_instances}()},
\code{\link{get_lookup_tables}()},
\code{\link{get_new_dimension_instances}()},
\code{\link{get_star_schema}()},
\code{\link{get_transformation_code}()},
\code{\link{get_transformation_file}()},
\code{\link{incremental_refresh}()},
\code{\link{update_according_to}()}
}
\concept{star database refresh functions}
