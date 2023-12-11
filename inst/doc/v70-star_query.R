## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, message=FALSE---------------------------------------
library(rolap)

## -----------------------------------------------------------------------------
class(mrs_db)

## -----------------------------------------------------------------------------
mrs_db |>
  draw_tables()

## -----------------------------------------------------------------------------
mrs_db_geo <- mrs_db |>
  define_geoattribute(
    dimension = "where",
    attribute = "city",
    from_attribute = c("long", "lat")
  )

## -----------------------------------------------------------------------------
mrs_db_geo <- mrs_db_geo |>
  define_geoattribute(
    dimension = "where",
    attribute = "state",
    from_layer = us_layer_state,
    by = "STUSPS"
  )

## -----------------------------------------------------------------------------
mrs_db_geo <- mrs_db_geo |>
  define_geoattribute(
    dimension = "where",
    attribute = "region",
    from_layer = us_layer_state,
    by = "DIVISION"
  )

## -----------------------------------------------------------------------------
mrs_db_geo_2 <- mrs_db_geo |>
  define_geoattribute(
    dimension = "where",
    attribute = "region",
    from_attribute = "state"
  )

## -----------------------------------------------------------------------------
sq <- mrs_db_geo |>
  star_query()

## -----------------------------------------------------------------------------
sq_1 <- sq |>
  select_fact(
    name = "mrs_age",
    measures = "all_deaths",
    agg_functions = "MAX"
  )

## -----------------------------------------------------------------------------
sq_2 <- sq |>
  select_fact(name = "mrs_age",
              measures = "all_deaths")

## -----------------------------------------------------------------------------
sq_3 <- sq |>
  select_fact(name = "mrs_age")

## -----------------------------------------------------------------------------
sq_4 <- sq |>
  select_fact(name = "mrs_age",
              measures = "all_deaths") |>
  select_fact(name = "mrs_cause")

## -----------------------------------------------------------------------------
sq_1 <- sq |>
  select_dimension(name = "where",
                   attributes = c("city", "state"))

## -----------------------------------------------------------------------------
sq_2 <- sq |>
  select_dimension(name = "where")

## -----------------------------------------------------------------------------
sq <- sq |>
  filter_dimension(name = "when", week <= " 3") |>
  filter_dimension(name = "where", city == "Bridgeport")

## -----------------------------------------------------------------------------
sq <- star_query(mrs_db_geo) |>
  select_dimension(name = "where",
                   attributes = c("region", "state")) |>
  select_dimension(name = "when",
                   attributes = "year") |>
  select_fact(name = "mrs_age",
              measures = "all_deaths") |>
  select_fact(name = "mrs_cause",
              measures = "all_deaths") |>
  filter_dimension(name = "when", week <= " 3" & year >= "2010")

mrs_db_geo_3 <- mrs_db_geo |>
  run_query(sq)

class(mrs_db_geo_3)

## -----------------------------------------------------------------------------
mrs_db_geo_3 |>
  draw_tables()

## -----------------------------------------------------------------------------
ft <- mrs_db_geo_3 |>
  as_single_tibble_list()
ft_age <- ft[["mrs_age"]]

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(ft_age, split.table = Inf)

## -----------------------------------------------------------------------------
pt <- pivottabler::qpvt(
  ft_age,
  c("=", "region"),
  c("year"),
  c("Number of Deaths" = "sum(all_deaths)")
)

pt$renderPivot()

## -----------------------------------------------------------------------------
gl_state <- mrs_db_geo_3 |>
  as_geolayer(attribute = "state")

## -----------------------------------------------------------------------------
layer_state <- gl_state |>
  get_layer()
layer_state

var_state <- gl_state |>
  get_variables()
var_state

## -----------------------------------------------------------------------------
plot(sf::st_geometry(layer_state))
text(
  sf::st_coordinates(sf::st_centroid(sf::st_geometry(layer_state))),
  labels = layer_state$state,
  pos = 3,
  cex = 0.5
)

## -----------------------------------------------------------------------------
layer_state_all <- gl_state |>
  get_layer(keep_all_variables_na = TRUE)

plot(sf::st_shift_longitude(sf::st_geometry(layer_state_all)))

## -----------------------------------------------------------------------------
gl_state |>
  get_variable_description(c("var_01", "var_10"))

vd <- gl_state |>
  get_variable_description()
vd[c("var_01", "var_10")]

## -----------------------------------------------------------------------------
var_state_2 <- var_state |>
  dplyr::filter(year == '2016')

## -----------------------------------------------------------------------------
gl_state_2 <- gl_state |>
  set_variables(var_state_2)

layer_state_2 <- gl_state_2 |>
  get_layer()

## -----------------------------------------------------------------------------
plot(sf::st_geometry(layer_state_2))
text(
  sf::st_coordinates(sf::st_centroid(sf::st_geometry(layer_state_2))),
  labels = layer_state_2$state,
  pos = 3,
  cex = 0.5
)

## -----------------------------------------------------------------------------
sq_2 <- star_query(mrs_db_geo) |>
  select_dimension(name = "where",
                   attributes = "state") |>
  select_fact(name = "mrs_cause",
              measures = c("pneumonia_and_influenza_deaths", "all_deaths")) |>
  filter_dimension(name = "when", year >= "2010")

## -----------------------------------------------------------------------------
mrs_db_geo_3 <- mrs_db_geo |>
  run_query(sq_2)

## -----------------------------------------------------------------------------
gl_state_3 <- mrs_db_geo_3 |>
  as_geolayer(attribute = "state")

## -----------------------------------------------------------------------------
gl_state_3 |>
  get_variable_description()

layer <- gl_state_3 |>
  get_layer()

layer$tpc_deaths <- (layer$var_2 / layer$var_1) * 100

plot(layer[, "tpc_deaths"], main = "% pneumonia and influenza")

## -----------------------------------------------------------------------------
gl_state_3 <- gl_state_3 |>
  set_layer(layer)

## -----------------------------------------------------------------------------
f <- gl_state_3 |>
  as_GeoPackage(dir = tempdir())

sf::st_layers(f)

