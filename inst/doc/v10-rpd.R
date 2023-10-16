## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, message=FALSE---------------------------------------
library(rolap)

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(ft_cause_rpd, split.table = Inf)

## -----------------------------------------------------------------------------
dput(colnames(ft_cause_rpd))

## -----------------------------------------------------------------------------
when <- dimension_schema(name = "When",
                         attributes = c("Year",
                                        "WEEK",
                                        "Week Ending Date"))
when_available <- dimension_schema(
  name = "When Available",
  attributes = c(
    "Data Availability Year",
    "Data Availability Week",
    "Data Availability Date"
  )
)
where <- dimension_schema(name = "where",
                          attributes = c("REGION",
                                         "State",
                                         "City"))
s <- star_schema() |>
  define_facts(fact_schema(
    name = "MRS Cause",
    measures = c("Pneumonia and Influenza Deaths",
                 "All Deaths")
  )) |>
  define_dimension(when) |>
  define_dimension(when_available) |>
  define_dimension(dimension_schema(
    name = "When Received",
    attributes = c("Reception Year",
                   "Reception Week",
                   "Reception Date")
  )) |>
  define_dimension(where)

## -----------------------------------------------------------------------------
db <- star_database(s, ft_cause_rpd) |>
  snake_case()

## ----echo = FALSE-------------------------------------------------------------
db_dm <- db |>
  as_dm_class(pk_facts = FALSE)
db_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

## ----results = "asis", echo = FALSE-------------------------------------------
l_db <- db |>
  as_tibble_list()
for (i in 1:length(l_db)) {
  pander::pandoc.table(l_db[[i]], split.table = Inf)
}

## -----------------------------------------------------------------------------
db_1 <- db |>
  role_playing_dimension(
    rpd = "when",
    roles = c("when_available", "when_received")
  )

## ----results = "asis", echo = FALSE-------------------------------------------
l_db <- db_1 |>
  as_tibble_list()
for (i in 1:length(l_db)) {
  pander::pandoc.table(l_db[[i]], split.table = Inf)
}

## -----------------------------------------------------------------------------
db_2 <- db |>
  role_playing_dimension(
    rpd = "when",
    roles = c("when_available", "when_received"),
    rpd_att_names = TRUE
  )

## ----echo = FALSE-------------------------------------------------------------
db_dm <- db_2 |>
  as_dm_class(pk_facts = FALSE)
db_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(ft_age_rpd, split.table = Inf)

## -----------------------------------------------------------------------------
dput(colnames(ft_age_rpd))

## -----------------------------------------------------------------------------
s_2 <- star_schema() |>
  define_facts(fact_schema(
    name = "MRS Age",
    measures = c(
      "Deaths"
    )
  )) |>
  define_dimension(when) |>
  define_dimension(when_available) |>
  define_dimension(dimension_schema(
    name = "When Arrived",
    attributes = c(
      "Arrival Year",
      "Arrival Week",
      "Arrival Date"
    )
  )) |>
  define_dimension(dimension_schema(
    name = "Who",
    attributes = c(
      "Age Range"
    )
  )) |>
  define_dimension(where)

## -----------------------------------------------------------------------------
db_3 <- star_database(s_2, ft_age_rpd) |>
  role_playing_dimension(
    rpd = "When Arrived",
    roles = c("When Available"),
    att_names = c("Year", "Week", "Week Ending Date")
  ) |>
  snake_case()

## ----results = "asis", echo = FALSE-------------------------------------------
l_db <- db_3 |>
  as_tibble_list()
for (i in 1:length(l_db)) {
  pander::pandoc.table(l_db[[i]], split.table = Inf)
}

## -----------------------------------------------------------------------------
ct <- constellation("MRS", db_2, db_3)

## ----echo = FALSE-------------------------------------------------------------
db_dm <- ct |>
  as_dm_class(pk_facts = FALSE)
db_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

## ----results = "asis", echo = FALSE-------------------------------------------
l_db <- ct |>
  as_tibble_list()
for (i in 1:length(l_db)) {
  pander::pandoc.table(l_db[[i]], split.table = Inf)
}

## -----------------------------------------------------------------------------
ct  |>
  get_role_playing_dimension_names()

