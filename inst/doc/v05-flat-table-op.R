## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
library(rolap)
#  mrs_ft: Declared as a variable instead of reading from the file due to problem building macos-latest (release)

## ----eval=FALSE---------------------------------------------------------------
# library(rolap)
# 
# file <-
#   system.file(
#     "extdata/mrs",
#     "mrs_122_us_cities_1962_2016.csv",
#     package = "rolap"
#   )
# 
# mrs_ft <-
#   read_flat_table_file(name = 'mrs', file, unknown_value = "Not available")

## ----results = "asis"---------------------------------------------------------
ft <- mrs_ft |> 
  get_table()

pander::pandoc.table(head(ft), split.table = Inf)

## ----results = "asis"---------------------------------------------------------
pander::pandoc.table(head(maps::us.cities), split.table = Inf)

## -----------------------------------------------------------------------------
usc_ft <-
  flat_table(name = 'us_cities', instances = maps::us.cities)

## -----------------------------------------------------------------------------
capital_status <- data.frame(
  code = c('0', '1', '2'),
  status = c('non-capital', 'capital', 'state capital')
)

cs_ft <-
  flat_table(name = 'capital_status', instances = capital_status)

## -----------------------------------------------------------------------------
mrs_ft |> 
  get_attribute_names()

mrs_ft |> 
  get_measure_names()

## -----------------------------------------------------------------------------
mrs_ft <- mrs_ft |>
  transform_to_measure(
    attributes = c(
      'Pneumonia and Influenza Deaths',
      'All Deaths',
      '<1 year (all cause deaths)',
      '1-24 years (all cause deaths)',
      '25-44 years',
      '45-64 years (all cause deaths)',
      '65+ years (all cause deaths)'
    )
  )

mrs_ft |> 
  get_attribute_names()

mrs_ft |> 
  get_measure_names()

## -----------------------------------------------------------------------------
mrs_ft <- mrs_ft |>
  transform_attribute_format(attributes = c('WEEK'),
                             width = 2)

## -----------------------------------------------------------------------------
table <- mrs_ft |>
  get_table()

sort(unique(table[['WEEK']]))[1:10]

## -----------------------------------------------------------------------------
usc_ft |> 
  get_attribute_names()

usc_ft |> 
  get_measure_names()

## -----------------------------------------------------------------------------
usc_ft <- usc_ft |>
  transform_to_attribute(measures = 'capital') |>
  transform_to_attribute(measures = 'pop',
                         width = 5) |>
  transform_to_attribute(measures = c('lat', 'long'),
                         width = 2,
                         decimal_places = 1)

## -----------------------------------------------------------------------------
cs_ft |> 
  get_attribute_names()

cs_ft |> 
  get_measure_names()

## -----------------------------------------------------------------------------
cs_ft <- cs_ft |>
  lookup_table(pk_attributes = 'code')

## -----------------------------------------------------------------------------
usc_ft <- usc_ft |>
  join_lookup_table(fk_attributes = 'capital', lookup = cs_ft)

## ----results = "asis"---------------------------------------------------------
ft <- usc_ft |> 
  get_table()

pander::pandoc.table(head(ft), split.table = Inf)

## -----------------------------------------------------------------------------
usc_ft <- usc_ft |>
  lookup_table(pk_attributes = 'name')

## ----results = "asis", echo=FALSE---------------------------------------------
ft <- mrs_ft |> 
  get_table()

pander::pandoc.table(head(ft), split.table = Inf)

## -----------------------------------------------------------------------------
# function to define a derived column
city_state <- function(table) {
  paste0(table$City, ' ', table$State)
}

mrs_ft_TMP <- mrs_ft |>
  add_custom_column(name = 'city_state', definition = city_state)

## -----------------------------------------------------------------------------
mrs_ft_TMP |>
  check_lookup_table(fk_attributes = 'city_state', lookup = usc_ft)

## -----------------------------------------------------------------------------
mrs_ft <- mrs_ft |>
  replace_empty_values()

## -----------------------------------------------------------------------------
mrs_ft <- mrs_ft |>
  add_custom_column(name = 'city_state', definition = city_state)

## -----------------------------------------------------------------------------
usc_ft <- usc_ft |>
  replace_attribute_values(
    attributes = 'name',
    old = c('WASHINGTON DC'),
    new = c('Washington DC')
  )

mrs_ft <- mrs_ft |>
  replace_attribute_values(
    attributes = c('City', 'city_state'),
    old = c('Wilimington', 'Wilimington DE'),
    new = c('Wilmington', 'Wilmington DE')
  )

## -----------------------------------------------------------------------------
mrs_ft |>
  check_lookup_table(fk_attributes = 'city_state', lookup = usc_ft)

## -----------------------------------------------------------------------------
mrs_ft <- mrs_ft |>
  join_lookup_table(fk_attributes = 'city_state', lookup = usc_ft)

## -----------------------------------------------------------------------------
mrs_ft |>
  get_attribute_names(as_definition = TRUE)

## -----------------------------------------------------------------------------
mrs_ft <- mrs_ft |>
  select_attributes(
    attributes = c(
      'Year',
      'WEEK',
      'Week Ending Date',
      'REGION',
      'State',
      'City',
      'city_state',
      'status',
      'pop',
      'lat',
      'long'
    )
  )

## -----------------------------------------------------------------------------
l_mrs_ft <- mrs_ft |>
  separate_measures(measures = list(
    c('Pneumonia and Influenza Deaths',
      'All Deaths'),
    c(
      '<1 year (all cause deaths)',
      '1-24 years (all cause deaths)',
      '25-44 years',
      '45-64 years (all cause deaths)',
      '65+ years (all cause deaths)'
    )
  ),
  names = c('mrs_cause', 'mrs_age'))

mrs_cause_ft <- l_mrs_ft[['mrs_cause']]
mrs_age_ft <- l_mrs_ft[['mrs_age']]

## -----------------------------------------------------------------------------
mrs_cause_ft <- mrs_cause_ft |>
  snake_case()

## ----results = "asis", echo=FALSE---------------------------------------------
ft <- mrs_age_ft |> 
  get_table()

pander::pandoc.table(head(ft), split.table = Inf)

cat(sprintf("Number of rows in the table: %d", nrow(ft)))

## -----------------------------------------------------------------------------
mrs_age_ft <- mrs_age_ft |>
  transform_to_values(attribute = 'age',
                      measure = 'all_deaths')

## -----------------------------------------------------------------------------
mrs_age_ft <- mrs_age_ft |>
  snake_case()

## -----------------------------------------------------------------------------
mrs_age_ft <- mrs_age_ft |>
  replace_string(
    attributes = 'age',
    string = ' (all cause deaths)',
    replacement = ''
  )

## ----results = "asis", echo=FALSE---------------------------------------------
ft <- mrs_age_ft |> 
  get_table()

pander::pandoc.table(head(ft), split.table = Inf)


cat(sprintf("Number of rows in the table: %d", nrow(ft)))

## -----------------------------------------------------------------------------
mrs_age_ft_TMP <- mrs_age_ft |>
  transform_from_values(
    attribute = 'age'
  )

## ----results = "asis", echo=FALSE---------------------------------------------
ft <- mrs_age_ft_TMP |> 
  get_table()

pander::pandoc.table(head(ft), split.table = Inf)

cat(sprintf("Number of rows in the table: %d", nrow(ft)))

## -----------------------------------------------------------------------------
mrs_cause_ft |>
  get_attribute_names(as_definition = TRUE)

mrs_cause_ft |>
  get_measure_names(as_definition = TRUE)

## -----------------------------------------------------------------------------
when <- dimension_schema(
  name = 'when',
  attributes = c(
    'year', 
    'week', 
    'week_ending_date'
  )
)
where <- dimension_schema(
  name = "where",
  attributes = c(
    'region',
    'state',
    'city',
    'city_state',
    'status',
    'pop',
    'lat',
    'long'
  )
)
s_cause <- star_schema() |>
  define_facts(fact_schema(
    name = 'mrs_cause',
    measures = c('pneumonia_and_influenza_deaths', 'all_deaths')
  )) |>
  define_dimension(when) |>
  define_dimension(where)

## -----------------------------------------------------------------------------
mrs_cause_db <- mrs_cause_ft |>
  as_star_database(s_cause)

## -----------------------------------------------------------------------------
db_dm <- mrs_cause_db |>
  as_dm_class(pk_facts = FALSE)
db_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

## -----------------------------------------------------------------------------
who <- dimension_schema(
  name = 'who',
  attributes = c(
    'age'
  )
)
s_age <- star_schema() |>
  define_facts(fact_schema(
    name = 'mrs_age',
    measures = c('all_deaths')
  )) |>
  define_dimension(when) |>
  define_dimension(where) |>
  define_dimension(who)

## -----------------------------------------------------------------------------
mrs_age_db <- mrs_age_ft |>
  as_star_database(s_age)

## -----------------------------------------------------------------------------
db_dm <- mrs_age_db |>
  as_dm_class(pk_facts = FALSE)
db_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

## ----example5-----------------------------------------------------------------
mrs_db <- constellation("mrs", mrs_cause_db, mrs_age_db)

## -----------------------------------------------------------------------------
db_dm <- mrs_db |>
  as_dm_class(pk_facts = FALSE)
db_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

