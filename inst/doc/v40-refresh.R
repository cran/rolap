## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, message=FALSE---------------------------------------
library(rolap)

## -----------------------------------------------------------------------------
library(rolap)

mrs_db |> 
  get_fact_names()

## -----------------------------------------------------------------------------
db_dm <- mrs_db |>
  as_dm_class(pk_facts = FALSE)
db_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

## ----echo=FALSE---------------------------------------------------------------
#  mrs_ft_new: Declared as a variable instead of reading from the file due to problem building macos-latest (release)

## ----eval=FALSE---------------------------------------------------------------
#  file <-
#    system.file(
#      "extdata/mrs",
#      "mrs_122_us_cities_1962_2016_new.csv",
#      package = "rolap"
#    )
#  
#  mrs_ft_new <-
#    read_flat_table_file(name = 'mrs new', file)

## ----results = "asis"---------------------------------------------------------
ft <- mrs_ft_new |> 
  get_table()

pander::pandoc.table(head(ft), split.table = Inf)

## -----------------------------------------------------------------------------
mrs_db_age_refresh <- mrs_ft_new |>
  update_according_to(mrs_db, star = "mrs_age")

## -----------------------------------------------------------------------------
db_dm <- mrs_db_age_refresh |>
  get_star_database() |>
  as_dm_class(pk_facts = FALSE)
db_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

## -----------------------------------------------------------------------------
mrs_db_cause_refresh <- mrs_ft_new |>
  update_according_to(mrs_db, star = "mrs_cause")

## -----------------------------------------------------------------------------
db_dm <- mrs_db_cause_refresh |>
  get_star_database() |>
  as_dm_class(pk_facts = FALSE)
db_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

## -----------------------------------------------------------------------------
mrs_db_age_refresh |>
  get_transformation_code()

## -----------------------------------------------------------------------------
code <- mrs_db_cause_refresh |>
  get_transformation_code()

## -----------------------------------------------------------------------------
mrs_db_age_refresh |>
  get_new_dimension_instances()

## -----------------------------------------------------------------------------
mrs_db_age_refresh |>
  get_existing_fact_instances()

mrs_db_cause_refresh |>
  get_existing_fact_instances()

## -----------------------------------------------------------------------------
l_db <- mrs_db |>
  as_tibble_list()

names <- names(l_db)
for (i in seq_along(l_db)){
  cat(sprintf("name: %s, %d rows\n", names[i], nrow(l_db[[i]])))
}

head(sort(l_db[['where']]$city), 15)

## -----------------------------------------------------------------------------
mrs_db_seg <- mrs_db

mrs_db <- mrs_db |>
  incremental_refresh(mrs_db_age_refresh) |>
  incremental_refresh(mrs_db_cause_refresh, existing_instances = "group")

## -----------------------------------------------------------------------------
l_db <- mrs_db |>
  as_tibble_list()

names <- names(l_db)
for (i in seq_along(l_db)){
  cat(sprintf("name: %s, %d rows\n", names[i], nrow(l_db[[i]])))
}

head(sort(l_db[['where']]$city), 15)

## -----------------------------------------------------------------------------
transform_instance_table <-
  function(instance_df,
           lookup_ft,
           definition_fun,
           star_sch) {
    ft <-
      flat_table(name = 'mrs',
                 instances = instance_df,
                 unknown_value = 'Not available') |>
      transform_to_measure(
        attributes = c(
          'Pneumonia and Influenza Deaths',
          'All Deaths',
          '<1 year (all cause deaths)',
          '1-24 years (all cause deaths)',
          '25-44 years',
          '45-64 years (all cause deaths)',
          '65+ years (all cause deaths)'
        ),
        k_sep = NULL,
        decimal_sep = NULL
      ) |>
      transform_attribute_format(
        attributes = 'WEEK',
        width = 2,
        decimal_places = 0,
        k_sep = ',',
        decimal_sep = '.'
      ) |>
      replace_empty_values(
        attributes = c('Year', 'WEEK', 'Week Ending Date', 'REGION', 'State', 'City'),
        empty_values = NULL
      ) |>
      add_custom_column(name = 'city_state',
                        definition = definition_fun) |>
      replace_attribute_values(
        attributes = c('City', 'city_state'),
        old = c('Wilimington', 'Wilimington DE'),
        new = c('Wilmington', 'Wilmington DE')
      ) |>
      join_lookup_table(fk_attributes = 'city_state',
                        lookup = lookup_ft) |>
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
      ) |>
      separate_measures(
        measures = list(
          c('Pneumonia and Influenza Deaths', 'All Deaths'),
          c(
            '<1 year (all cause deaths)',
            '1-24 years (all cause deaths)',
            '25-44 years',
            '45-64 years (all cause deaths)',
            '65+ years (all cause deaths)'
          )
        ),
        names = c('mrs_cause', 'mrs_age'),
        na_rm = TRUE
      ) |>
      magrittr::extract2('mrs_cause') |>
      snake_case() |>
      as_star_database(schema = star_sch)
    
    ft
  }

## -----------------------------------------------------------------------------
instance_df <- mrs_ft_new |> 
  get_table()
  
lookup_list <- mrs_db_cause_refresh |> 
  get_lookup_tables()

star_sch <- mrs_db_cause_refresh |> 
  get_star_schema()

# function to define a derived column
city_state <- function(table) {
  paste0(table$City, ' ', table$State)
}

mrs_db_cause_transf <-
  transform_instance_table(
    instance_df = instance_df,
    lookup_ft = lookup_list[['us_cities']],
    definition_fun = city_state,
    star_sch = star_sch
  )

## -----------------------------------------------------------------------------
transform_instance_table_2 <-
  function(instance_df,
           lookup_ft,
           definition_fun,
           star_sch) {
    ft <-
      flat_table(name = 'mrs',
                 instances = instance_df,
                 unknown_value = 'Not available') |>
      transform_to_measure(
        attributes = c(
          'Pneumonia and Influenza Deaths',
          'All Deaths',
          '<1 year (all cause deaths)',
          '1-24 years (all cause deaths)',
          '25-44 years',
          '45-64 years (all cause deaths)',
          '65+ years (all cause deaths)'
        ),
        k_sep = NULL,
        decimal_sep = NULL
      ) |>
      transform_attribute_format(
        attributes = 'WEEK',
        width = 2,
        decimal_places = 0,
        k_sep = ',',
        decimal_sep = '.'
      ) |>
      replace_empty_values(
        attributes = c('Year', 'WEEK', 'Week Ending Date', 'REGION', 'State', 'City'),
        empty_values = NULL
      ) |>
      add_custom_column(name = 'city_state',
                        definition = definition_fun) |>
      replace_attribute_values(
        attributes = c('City', 'city_state'),
        old = c('Wilimington', 'Wilimington DE'),
        new = c('Wilmington', 'Wilmington DE')
      ) |>
      join_lookup_table(fk_attributes = 'city_state',
                        lookup = lookup_ft) |>
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
      ) |>
      separate_measures(
        measures = list(
          c('Pneumonia and Influenza Deaths', 'All Deaths'),
          c(
            '<1 year (all cause deaths)',
            '1-24 years (all cause deaths)',
            '25-44 years',
            '45-64 years (all cause deaths)',
            '65+ years (all cause deaths)'
          )
        ),
        names = c('mrs_cause', 'mrs_age'),
        na_rm = TRUE
      ) |>
      magrittr::extract2('mrs_age') |>
      transform_to_values(
        attribute = 'age',
        measure = 'all_deaths',
        id_reverse = NULL,
        na_rm = TRUE
      ) |>
      snake_case() |>
      replace_string(attributes = 'age',
                     string = ' (all cause deaths)',
                     replacement = NULL) |>
      as_star_database(schema = star_sch)
    
    ft
  }

star_sch <- mrs_db_age_refresh |>
  get_star_schema()

mrs_db_age_transf <-
  transform_instance_table_2(
    instance_df = instance_df,
    lookup_ft = lookup_list[['us_cities']],
    definition_fun = city_state,
    star_sch = star_sch
  )

## -----------------------------------------------------------------------------
mrs_db_cause_transf_refresh <- mrs_ft_new |>
  update_according_to(mrs_db_seg, star = "mrs_cause", sdb_operations = mrs_db_cause_transf)

mrs_db_age_transf_refresh <- mrs_ft_new |>
  update_according_to(mrs_db_seg, star = "mrs_age", sdb_operations = mrs_db_age_transf)

## -----------------------------------------------------------------------------
l_db <- mrs_db_seg |>
  as_tibble_list()

names <- names(l_db)
for (i in seq_along(l_db)){
  cat(sprintf("name: %s, %d rows\n", names[i], nrow(l_db[[i]])))
}

head(sort(l_db[['where']]$city), 15)

## -----------------------------------------------------------------------------
mrs_db_seg <- mrs_db_seg |>
  incremental_refresh(mrs_db_age_transf_refresh, replace_transformations = TRUE) |>
  incremental_refresh(
    mrs_db_cause_transf_refresh,
    existing_instances = "group",
    replace_transformations = TRUE
  )

## -----------------------------------------------------------------------------
l_db <- mrs_db_seg |>
  as_tibble_list()

names <- names(l_db)
for (i in seq_along(l_db)){
  cat(sprintf("name: %s, %d rows\n", names[i], nrow(l_db[[i]])))
}

head(sort(l_db[['where']]$city), 15)

