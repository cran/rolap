#' Get star database
#'
#' It obtains the star database: For updates, the one defined from the data; for
#' constellations, the one indicated by the parameter.
#'
#' @param db A `star_database_update` object.
#' @param name A string, star database name (fact name).
#'
#' @return A `star_database` object.
#'
#' @family star database refresh functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' f1 <- flat_table('ft_num', ft_cause_rpd) |>
#'   as_star_database(mrs_cause_schema_rpd)
#' f2 <- flat_table('ft_num2', ft_cause_rpd) |>
#'   update_according_to(f1)
#' st <- f2 |>
#'   get_star_database()
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#' ct <- constellation("MRS", db1, db2)
#' names <- ct |>
#'   get_fact_names()
#' st <- ct |>
#'   get_star_database(names[1])
#'
#' @export
get_star_database <- function(db, name) UseMethod("get_star_database")

#' Transform names according to the snake case style
#'
#' For flat tables, transform attribute and measure names according to the snake
#' case style. For star databases, transform fact, dimension, measures, and
#' attribute names according to the snake case style.
#'
#' This style is suitable if we are going to work with databases.
#'
#' @param db A `flat_table` or `star_database` object.
#'
#' @return A `flat_table` or `star_database` object.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#'
#' ft <- flat_table('iris', iris) |>
#'   snake_case()
#'
#' @export
snake_case <- function(db) UseMethod("snake_case")


#' Get the names of the attributes
#'
#' Obtain the names of the attributes in a flat table or a dimension in a star
#' database.
#'
#' If indicated, names can be obtained in alphabetical order or as a vector
#' definition in R
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, dimension name.
#' @param ordered A boolean, sort names alphabetically.
#' @param as_definition A boolean, get the names as a vector definition in R.
#'
#' @return A vector of strings or a string, attribute names.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' names <- star_database(mrs_cause_schema, ft_num) |>
#'   get_attribute_names(name = "where")
#'
#' names <- flat_table('iris', iris) |>
#'   get_attribute_names()
#'
#' @export
get_attribute_names <- function(db, name, ordered, as_definition) UseMethod("get_attribute_names")


#' Get the names of the measures
#'
#' Obtain the names of the measures in a flat table or in a star database.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, dimension name.
#' @param ordered A boolean, sort names alphabetically.
#' @param as_definition A boolean, get the names as a vector definition in R.
#'
#' @return A vector of strings or a string, measure names.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' names <- star_database(mrs_cause_schema, ft_num) |>
#'   get_measure_names()
#'
#' names <- flat_table('iris', iris) |>
#'   get_measure_names()
#'
#' @export
get_measure_names <- function(db, name, ordered, as_definition) UseMethod("get_measure_names")


#' Rename attributes
#'
#' Rename attributes in a flat table or a dimension in a star database.
#'
#' To rename the attributes there are three possibilities: 1) give only one vector
#' with the new names for all the attributes; 2) a vector of old names and another
#' of new names that must correspond; 3) a vector of new names whose names are
#' the old names they replace.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, dimension name.
#' @param old A vector of names.
#' @param new A vector of names.
#'
#' @return A `flat_table` or `star_database` object.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   set_attribute_names(
#'     name = "where",
#'     new = c(
#'       "Region",
#'       "State",
#'       "City"
#'     )
#'   )
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   set_attribute_names(name = "where",
#'                       old = "REGION",
#'                       new = "Region")
#'
#' new <- "Region"
#' names(new) <- "REGION"
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   set_attribute_names(name = "where",
#'                       new = new)
#'
#' ft <- flat_table('iris', iris) |>
#'   set_attribute_names(
#'     old = 'Species',
#'     new = 'species')
#'
#' new <- "species"
#' names(new) <- "Species"
#' ft <- flat_table('iris', iris) |>
#'   set_attribute_names(
#'     new = new)
#'
#' @export
set_attribute_names <- function(db, name, old, new) UseMethod("set_attribute_names")


#' Rename measures
#'
#' Rename measures in a flat table or in facts in a star database.
#'
#' To rename the measures there are three possibilities: 1) give only one vector
#' with the new names for all the measures; 2) a vector of old names and another
#' of new names that must correspond; 3) a vector of new names whose names are
#' the old names they replace.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, fact name.
#' @param old A vector of names.
#' @param new A vector of names.
#'
#' @return A `flat_table` or `star_database` object.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   set_measure_names(
#'     new = c(
#'       "Pneumonia and Influenza",
#'       "All",
#'       "Rows Aggregated"
#'     )
#'   )
#'
#' ft <- flat_table('iris', iris) |>
#'   set_measure_names(
#'     old = c('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width'),
#'     new = c('pl', 'pw', 'ls', 'sw'))
#'
#' new <- c('pl', 'pw', 'ls', 'sw')
#' names(new) <- c('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width')
#' ft <- flat_table('iris', iris) |>
#'   set_measure_names(
#'     new = new)
#'
#' @export
set_measure_names <- function(db, name, old, new) UseMethod("set_measure_names")


#' Get similar attribute values combination
#'
#' Get sets of attribute values that differ only by tildes, spaces, or punctuation
#' marks, for the combination of the given set of attributes. If no attributes are
#' indicated, they are all considered together.
#'
#' For star databases, a list of dimensions can be indicated, otherwise it
#' considers all dimensions. If a dimension is indicated, a list of attributes
#' to be considered in it can also be indicated.
#'
#' You can indicate that the numbers are ignored to make the comparison.
#'
#' If a name is indicated in the `col_as_vector` parameter, it includes a column
#' with the data in vector form to be used in other functions.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, dimension name.
#' @param attributes A vector of strings, attribute names.
#' @param exclude_numbers A boolean, exclude numbers from comparison.
#' @param col_as_vector A string, name of the column to include a vector of values.
#'
#' @return A vector of `tibble` objects with similar instances.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' instances <- star_database(mrs_cause_schema, ft_num) |>
#'   get_similar_attribute_values(name = "where")
#'
#' db <- star_database(mrs_cause_schema, ft_num)
#' db$dimensions$where$table$City[2] <- " BrId  gEport "
#' instances <- db |>
#'   get_similar_attribute_values("where")
#'
#' db <- star_database(mrs_cause_schema, ft_num)
#' db$dimensions$where$table$City[2] <- " BrId  gEport "
#' instances <- db |>
#'   get_similar_attribute_values("where",
#'     attributes = c("City", "State"),
#'     col_as_vector = "As a vector")
#'
#' ft <- flat_table('iris', iris)
#' ft$table$Species[20] <- "se.Tosa."
#' ft$table$Species[60] <- "Versicolor"
#' instances <- ft |>
#'   get_similar_attribute_values()
#'
#' @export
get_similar_attribute_values <- function(db, name, attributes, exclude_numbers, col_as_vector) UseMethod("get_similar_attribute_values")


#' Get similar values for individual attributes
#'
#' Get sets of attribute values for individual attributes that differ only by
#' tildes, spaces, or punctuation marks. If no attributes are indicated, all are
#' considered.
#'
#' For star databases, if no dimension name is indicated, all dimensions are
#' considered.
#'
#' You can indicate that the numbers are ignored to make the comparison.
#'
#' If a name is indicated in the `col_as_vector` parameter, it includes a column
#' with the data in vector form to be used in other functions.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A vector of strings, dimension names.
#' @param attributes A vector of strings, attribute names.
#' @param exclude_numbers A boolean, exclude numbers from comparison.
#' @param col_as_vector A string, name of the column to include a vector of values.
#'
#' @return A vector of `tibble` objects with similar instances.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' instances <- star_database(mrs_cause_schema, ft_num) |>
#'   get_similar_attribute_values_individually(name = c("where", "when"))
#'
#' instances <- star_database(mrs_cause_schema, ft_num) |>
#'   get_similar_attribute_values_individually()
#'
#' ft <- flat_table('iris', iris)
#' ft$table$Species[20] <- "se.Tosa."
#' ft$table$Species[60] <- "Versicolor"
#' instances <- ft |>
#'   get_similar_attribute_values_individually()
#'
#' @export
get_similar_attribute_values_individually <- function(db, name, attributes, exclude_numbers, col_as_vector) UseMethod("get_similar_attribute_values_individually")


#' Get unique attribute values
#'
#' Get unique set of values for the given attributes. If no attributes are
#' indicated, all are considered.
#'
#' If we work on a star database, a dimension must be indicated.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, dimension name.
#' @param attributes A vector of strings, attribute names.
#' @param col_as_vector A string, name of the column to include a vector of values.
#'
#' @return A vector of `tibble` objects with unique instances.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' instances <- star_database(mrs_cause_schema, ft_num) |>
#'   get_unique_attribute_values()
#'
#' instances <- star_database(mrs_cause_schema, ft_num) |>
#'   get_unique_attribute_values(name = "where")
#'
#' instances <- star_database(mrs_cause_schema, ft_num) |>
#'   get_unique_attribute_values("where",
#'     attributes = c("REGION", "State"))
#'
#' instances <- flat_table('iris', iris) |>
#'   get_unique_attribute_values()
#'
#' @export
get_unique_attribute_values <- function(db, name, attributes, col_as_vector) UseMethod("get_unique_attribute_values")


#' Replace instance values
#'
#' Given the values of a possible instance, for that combination, replace them
#' with the new data values.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, dimension name.
#' @param attributes A vector of strings, attribute names.
#' @param old A vector of values.
#' @param new A vector of values.
#'
#' @return A `flat_table` or `star_database` object.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   replace_attribute_values(name = "where",
#'     old = c('1', 'CT', 'Bridgeport'),
#'     new = c('1', 'CT', 'Hartford'))
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   replace_attribute_values(name = "where",
#'                            attributes = c('REGION', 'State'),
#'                            old = c('1', 'CT'),
#'                            new = c('2', 'CT'))
#'
#' ft <- flat_table('iris', iris) |>
#'   replace_attribute_values(
#'     attributes = 'Species',
#'     old = c('setosa'),
#'     new = c('versicolor')
#'   )
#'
#' @export
replace_attribute_values <- function(db, name, attributes, old, new) UseMethod("replace_attribute_values")

