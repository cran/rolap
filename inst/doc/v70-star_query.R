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
sq <- mrs_db |>
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
sq <- star_query(mrs_db) |>
  select_dimension(name = "where",
                   attributes = c("region", "state")) |>
  select_dimension(name = "when",
                   attributes = "year") |>
  select_fact(name = "mrs_age",
              measures = "all_deaths") |>
  select_fact(name = "mrs_cause",
              measures = "all_deaths") |>
  filter_dimension(name = "when", week <= " 3" & year >= "2010")

mrs_db_2 <- mrs_db |>
  run_query(sq)

class(mrs_db_2)

## -----------------------------------------------------------------------------
mrs_db_2 |>
  draw_tables()

## -----------------------------------------------------------------------------
ft <- mrs_db_2 |>
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

