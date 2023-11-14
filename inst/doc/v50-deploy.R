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

## -----------------------------------------------------------------------------
mrs_rdb_file <- tempfile()

## -----------------------------------------------------------------------------
mrs_sqlite_file <- tempfile("mrs", fileext = ".sqlite")

## -----------------------------------------------------------------------------
mrs_sqlite_connect <- function() {
  DBI::dbConnect(RSQLite::SQLite(),
                 dbname = mrs_sqlite_file)
}

mrs_sqlite_disconnect <- function(con) {
  DBI::dbDisconnect(con)
}

## -----------------------------------------------------------------------------
mrs_db <- mrs_db |>
  deploy(
    name = "mrs_sqlite",
    connect = mrs_sqlite_connect,
    disconnect = mrs_sqlite_disconnect,
    file = mrs_rdb_file
  )

## -----------------------------------------------------------------------------
l_db <- mrs_db |>
  as_tibble_list()

names <- sort(names(l_db))
for (name in names){
  cat(sprintf("name: %s, %d rows\n", name, nrow(l_db[[name]])))
}

## -----------------------------------------------------------------------------
mrs_con <- mrs_sqlite_connect()

tables <- DBI::dbListTables(mrs_con)
for (t in tables) {
  res <- DBI::dbGetQuery(mrs_con, sprintf('SELECT COUNT(*) FROM `%s`', t))
  cat(sprintf("name: %s, %d rows\n", t, res[[1]]))
}

mrs_sqlite_disconnect(mrs_con)

## -----------------------------------------------------------------------------
mrs_sqlite_file_2 <- tempfile("mrs", fileext = ".sqlite")

## -----------------------------------------------------------------------------
mrs_sqlite_connect_2 <- function() {
  DBI::dbConnect(RSQLite::SQLite(),
                 dbname = mrs_sqlite_file_2)
}

## -----------------------------------------------------------------------------
mrs_db <- mrs_db |>
  deploy(
    name = "mrs_sqlite_2",
    connect = mrs_sqlite_connect_2
  )

## -----------------------------------------------------------------------------
mrs_db |>
  get_deployment_names()

## -----------------------------------------------------------------------------
ft <- mrs_ft_new |> 
  get_table()

ft

## -----------------------------------------------------------------------------
mrs_db_age_refresh <- mrs_ft_new |>
  update_according_to(mrs_db, star = "mrs_age")

mrs_db_cause_refresh <- mrs_ft_new |>
  update_according_to(mrs_db, star = "mrs_cause")

## -----------------------------------------------------------------------------
mrs_db <- mrs_db |>
  incremental_refresh(mrs_db_age_refresh) |>
  incremental_refresh(mrs_db_cause_refresh, existing_instances = "group")

## -----------------------------------------------------------------------------
l_db <- mrs_db |>
  as_tibble_list()

names <- sort(names(l_db))
for (name in names){
  cat(sprintf("name: %s, %d rows\n", name, nrow(l_db[[name]])))
}

## -----------------------------------------------------------------------------
mrs_con <- mrs_sqlite_connect()

tables <- DBI::dbListTables(mrs_con)
for (t in tables) {
  res <- DBI::dbGetQuery(mrs_con, sprintf('SELECT COUNT(*) FROM `%s`', t))
  cat(sprintf("name: %s, %d rows\n", t, res[[1]]))
}

mrs_sqlite_disconnect(mrs_con)

## -----------------------------------------------------------------------------
mrs_con_2 <- mrs_sqlite_connect_2()

tables <- DBI::dbListTables(mrs_con_2)
for (t in tables) {
  res <- DBI::dbGetQuery(mrs_con_2, sprintf('SELECT COUNT(*) FROM `%s`', t))
  cat(sprintf("name: %s, %d rows\n", t, res[[1]]))
}

mrs_sqlite_disconnect(mrs_con_2)

## -----------------------------------------------------------------------------
mrs_db_new <- load_star_database(mrs_rdb_file)

identical(mrs_db, mrs_db_new)

## ----echo = FALSE-------------------------------------------------------------
unlink(mrs_rdb_file)
unlink(mrs_sqlite_file)
unlink(mrs_sqlite_file_2)

