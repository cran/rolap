% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoattribute.R
\name{check_geoattribute_geometry}
\alias{check_geoattribute_geometry}
\alias{check_geoattribute_geometry.star_database}
\title{Check a \code{geoattribute} geometry instances.}
\usage{
check_geoattribute_geometry(db, dimension, attribute, geometry)

\method{check_geoattribute_geometry}{star_database}(
  db,
  dimension = NULL,
  attribute = NULL,
  geometry = "polygon"
)
}
\arguments{
\item{db}{A \code{star_database} object.}

\item{dimension}{A string, dimension name.}

\item{attribute}{A vector, attribute names.}

\item{geometry}{A string, geometry name ('point' or 'polygon').}
}
\value{
A \code{tibble}.
}
\description{
Get unrelated instances of a \code{geoattribute} for a geometry.
}
\details{
We obtain the values of the dimension attribute that do not have an associated
geographic element of the indicated geometry.

If there is only one geoattribute defined, neither the dimension nor the attribute
must be indicated.
}
\examples{

db <- mrs_db |>
  define_geoattribute(
    dimension = "where",
    attribute = "state",
    from_layer = us_layer_state,
    by = "STUSPS"
  )

instances <- check_geoattribute_geometry(db,
                                         dimension = "where",
                                         attribute = "state")

}
\seealso{
Other star database geographic attributes: 
\code{\link{define_geoattribute}()},
\code{\link{get_geoattribute_geometries}()},
\code{\link{get_geoattributes}()},
\code{\link{get_layer_geometry}()},
\code{\link{get_point_geometry}()},
\code{\link{summarize_layer}()}
}
\concept{star database geographic attributes}
