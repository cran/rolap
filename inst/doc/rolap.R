## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, message=FALSE---------------------------------------
library(rolap)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(ft, split.table = Inf)

## -----------------------------------------------------------------------------
library(rolap)

ft_num <- ft |>
  dplyr::mutate(`Pneumonia and Influenza Deaths` = as.integer(`Pneumonia and Influenza Deaths`)) |>
  dplyr::mutate(`All Deaths` = as.integer(`All Deaths`))

ft_age <- ft |>
  dplyr::select(-`Pneumonia and Influenza Deaths`,-`All Deaths`) |>
  tidyr::gather("Age", "All Deaths", 7:11) |>
  dplyr::mutate(`All Deaths` = as.integer(`All Deaths`)) |>
  dplyr::mutate(Age = stringr::str_replace(Age, " \\(all cause deaths\\)", ""))

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(ft_age, 15), split.table = Inf)

## -----------------------------------------------------------------------------
dput(colnames(ft_num))

dput(colnames(ft_age))

## -----------------------------------------------------------------------------
s <- star_schema()

## -----------------------------------------------------------------------------
# definition 1
when <- dimension_schema(name = "When",
                         attributes = c("Year",
                                        "WEEK"))
s_age <- star_schema() |>
  define_dimension(when)

# definition 2
s_age <- star_schema() |>
  define_dimension(name = "When",
                         attributes = c("Year",
                                        "WEEK"))

## -----------------------------------------------------------------------------
# definition 1
mrs_cause <- fact_schema(name = "MRS Cause",
               measures = c("Pneumonia and Influenza Deaths",
                            "All Deaths"))
s_cause <- star_schema() |>
  define_facts(mrs_cause)

# definition 2
s_cause <- star_schema() |>
  define_facts(name = "MRS Cause",
               measures = c("Pneumonia and Influenza Deaths",
                            "All Deaths"))

## -----------------------------------------------------------------------------
s_cause <- star_schema() |>
  define_facts(name = "MRS Cause",
               measures = c("Pneumonia and Influenza Deaths",
                            "All Deaths"),
               agg_functions = c("MAX", "SUM"),
               nrow_agg = "Num" 
               )

## -----------------------------------------------------------------------------
s_cause <- star_schema() |>
  define_facts(name = "MRS Cause",
               measures = c("Pneumonia and Influenza Deaths",
                            "All Deaths"),
               nrow_agg = "Num") |>
  define_dimension(name = "When",
                         attributes = c("Year",
                                        "WEEK",
                                        "Week Ending Date")) |>
  define_dimension(name = "Where",
                          attributes = c("REGION",
                                         "State",
                                         "City"))

## -----------------------------------------------------------------------------
db_cause <- star_database(s_cause, ft_num)

## -----------------------------------------------------------------------------
l_cause <- db_cause |>
  snake_case() |>
  as_tibble_list()

## ---- results = "asis", echo = FALSE------------------------------------------
for (i in 1:length(l_cause)) {
  pander::pandoc.table(l_cause[[i]], split.table = Inf)
}

## -----------------------------------------------------------------------------
nrow(ft_num)

nrow(l_cause[[3]])

## -----------------------------------------------------------------------------
when <- dimension_schema(name = "When",
                         attributes = c("Year"))

where <- dimension_schema(name = "Where",
                          attributes = c("State",
                                         "City"))
s_cause <- star_schema() |>
  define_facts(name = "MRS Cause",
               measures = c("Pneumonia and Influenza Deaths",
                            "All Deaths")) |>
  define_dimension(when) |>
  define_dimension(where)

ft_num2 <- ft_num |>
  dplyr::filter(Year > "1962") |>
  dplyr::filter(City == "Boston" | City == "Bridgeport")

db_cause <- star_database(s_cause, ft_num2) |>
  snake_case()

l_cause <- db_cause |>
  as_tibble_list()

## ---- results = "asis", echo = FALSE------------------------------------------
for (i in 1:length(l_cause)) {
  pander::pandoc.table(l_cause[[i]], split.table = Inf)
}

## -----------------------------------------------------------------------------
s_age <- star_schema() |>
  define_facts(name = "MRS Age",
               measures = c("All Deaths")) |>
  define_dimension(when) |>
  define_dimension(where) |>
  define_dimension(name = "Who",
                         attributes = c("Age"))

ft_age2 <- ft_age |>
  dplyr::filter(Year < "1964") |>
  dplyr::filter(City != "Boston" & City != "Bridgeport")

db_age <- star_database(s_age, ft_age2) |>
  snake_case()

l_age <- db_age |>
  as_tibble_list()

## ---- results = "asis", echo = FALSE------------------------------------------
for (i in 1:length(l_age)) {
  pander::pandoc.table(l_age[[i]], split.table = Inf)
}

## ----example5-----------------------------------------------------------------
ct <- constellation("MRS", list(db_cause, db_age))

## ----example6-----------------------------------------------------------------
lc <- ct |>
  as_tibble_list()

## ---- results = "asis", echo = FALSE------------------------------------------
for (i in 1:length(lc)) {
  pander::pandoc.table(lc[[i]], split.table = Inf)
}

## -----------------------------------------------------------------------------
db_cause |>
  as_tibble_list()

## -----------------------------------------------------------------------------
ct |>
  as_tibble_list()

## -----------------------------------------------------------------------------
# star database
db_cause_dm <- db_cause |>
  as_dm_class()
class(db_cause_dm)
db_cause_dm

## -----------------------------------------------------------------------------
# constellation
ct_dm <- ct |>
  as_dm_class(pk_facts = TRUE)
class(ct_dm)
ct_dm

## -----------------------------------------------------------------------------
ct_dm |> 
  dm::dm_draw(rankdir = "LR", view_type = "all")

## -----------------------------------------------------------------------------
db <- DBI::dbConnect(RSQLite::SQLite())
ct_dm_db <- dm::copy_dm_to(db, ct_dm)
ct_dm_db
DBI::dbDisconnect(db)

