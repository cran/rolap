## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(RMariaDB)

ccs_db <- RMariaDB::dbConnect(
  RMariaDB::MariaDB(),
  username = "guest",
  password = "relational",
  dbname = "ccs",
  host = "relational.fit.cvut.cz"
)

## ----message=FALSE------------------------------------------------------------
library(dm)

ccs_dm <- dm::dm_from_con(ccs_db, learn_keys = TRUE)

## -----------------------------------------------------------------------------
ccs_dm |>
  dm::dm_draw(view_type = "all")

## -----------------------------------------------------------------------------
ccs_dm$transactions

## -----------------------------------------------------------------------------
ccs_sel_dm <-
  ccs_dm[c('transactions_1k', 'customers', 'gasstations', 'products')] |>
  dm::dm_add_fk(transactions_1k, CustomerID, customers) |>
  dm::dm_add_fk(transactions_1k, GasStationID, gasstations) |>
  dm::dm_add_fk(transactions_1k, ProductID, products)

## -----------------------------------------------------------------------------
ccs_sel_dm |>
  dm::dm_draw(view_type = "all")

## ----message=FALSE------------------------------------------------------------
transactions_ft <- ccs_sel_dm |>
  dm::dm_flatten_to_tbl(transactions_1k, .recursive = TRUE) |>
  dm::collect()

class(transactions_ft)
nrow(transactions_ft)
dput(colnames(transactions_ft))

## -----------------------------------------------------------------------------
DBI::dbDisconnect(ccs_db)

## -----------------------------------------------------------------------------
length(unique(transactions_ft$TransactionID))
min(transactions_ft$TransactionID)
max(transactions_ft$TransactionID)

## -----------------------------------------------------------------------------
nrow(unique(transactions_ft[, c("Date", "Time", "CustomerID")]))
nrow(unique(transactions_ft[, c("Date", "Time", "CardID")]))
nrow(unique(transactions_ft[, c("Date", "Time", "CardID", "ProductID")]))

## -----------------------------------------------------------------------------
transactions_ft <- transactions_ft |>
  dplyr::mutate(Hour = format(as.POSIXct(Time), format = "%H"))  |>
  dplyr::mutate(`Processing Date` = format(as.POSIXct(Date) + 
    lubridate::days(2), format = "%Y-%m-%d"))

## ----setup--------------------------------------------------------------------
library(rolap)

s_finest <- rolap::star_schema() |>
  rolap::define_facts(name = "Transaction Line",
    measures = c("Amount", "Price")) |>
  rolap::define_dimension(name = "Transaction",
    attributes = c("Date", "Time", "CardID")) |>
  rolap::define_dimension(name = "Who",
    attributes = c("CardID", "CustomerID", "Segment.customers",
                   "Currency")) |>
  rolap::define_dimension(name = "Where",
    attributes = c("GasStationID", "ChainID", "Country",
                   "Segment.gasstations")) |>
  rolap::define_dimension(name = "When",
    attributes = c("Date", "Hour")) |>
  rolap::define_dimension(name = "When Moment",
    attributes = c("Time", "Hour")) |>
  rolap::define_dimension(name = "When Processed",
    attributes = c("Processing Date", "Hour")) |>
  rolap::define_dimension(name = "What",
    attributes = c("Description"))

## -----------------------------------------------------------------------------
db_finest <- rolap::star_database(s_finest, transactions_ft)

## -----------------------------------------------------------------------------
db_finest <- db_finest |>
  rolap::snake_case() |>
  rolap::set_attribute_names(
    name = "who",
    new = c(
      "card",
      "customer",
      "segment",
      "currency"
    )
  ) |>
  rolap::set_attribute_names(
    name = "where",
    new = c(
      "gas_station",
      "chain",
      "country",
      "segment"
    )
  ) |>
  rolap::set_attribute_names(
    name = "what",
    new = c(
      "product"
    )
  )

## -----------------------------------------------------------------------------
db_tl <- db_finest |>
  rolap::as_tibble_list()

names <- names(db_tl)
for (i in seq_along(db_tl)){
  cat(sprintf("name: %s, %d rows\n", names[i], nrow(db_tl[[i]])))
}

## -----------------------------------------------------------------------------
db_dm <- db_finest |>
  rolap::as_dm_class(pk_facts = FALSE)

tables <- db_finest |>
  rolap::get_table_names()

# Degenerate dimension
tables <- base::setdiff(tables, 'transaction')

db_finest_dm <- db_dm[tables]

## -----------------------------------------------------------------------------
db_finest_dm |> 
  dm::dm_draw(view_type = "all")

## -----------------------------------------------------------------------------
max(db_tl[['transaction_line']]$nrow_agg)
sum(db_tl[['transaction_line']]$nrow_agg)

## -----------------------------------------------------------------------------
db_finest <- db_finest |>
  rolap::role_playing_dimension(
    rpd = "when",
    roles = c("when_processed")
  )

## -----------------------------------------------------------------------------
db_tl <- db_finest |>
  rolap::as_tibble_list()

names <- names(db_tl)
for (i in seq_along(db_tl)){
  cat(sprintf("name: %s, %d rows\n", names[i], nrow(db_tl[[i]])))
}

## -----------------------------------------------------------------------------
my_db <- DBI::dbConnect(RSQLite::SQLite())
my_db_finest_dm <- dm::copy_dm_to(my_db, db_finest_dm)
my_db_finest_dm
DBI::dbDisconnect(my_db)

## -----------------------------------------------------------------------------
transactions_ft <- transactions_ft |>
  dplyr::mutate(`Payment Date` = format(as.POSIXct(Date) + 
    lubridate::days(1), format = "%Y-%m-%d"))

## -----------------------------------------------------------------------------
s_summary <- rolap::star_schema() |>
  rolap::define_facts(name = "Transaction Summary",
    measures = c("Amount", "Price"),
    nrow_agg = "Transactions") |>
  rolap::define_dimension(name = "Who Segment",
    attributes = c("Segment.customers",
                   "Currency")) |>
  rolap::define_dimension(name = "Where Chain",
    attributes = c("ChainID", "Country",
                   "Segment.gasstations")) |>
  rolap::define_dimension(name = "When",
    attributes = c("Date", "Hour")) |>
  rolap::define_dimension(name = "When Paid",
    attributes = c("Payment Date", "Hour")) |>
  rolap::define_dimension(name = "What",
    attributes = c("Description"))

## -----------------------------------------------------------------------------
db_summary <- rolap::star_database(s_summary, transactions_ft) |>
  rolap::snake_case() |>
  rolap::set_attribute_names(
    name = "who_segment",
    new = c(
      "segment",
      "currency"
    )
  ) |>
  rolap::set_attribute_names(
    name = "where_chain",
    new = c(
      "chain",
      "country",
      "segment"
    )
  ) |>
  rolap::set_attribute_names(
    name = "what",
    new = c(
      "product"
    )
  ) |>
  rolap::role_playing_dimension(
    rpd = "when",
    roles = c("when_paid")
  )

## -----------------------------------------------------------------------------
db_tl <- db_summary |>
  rolap::as_tibble_list()

names <- names(db_tl)
for (i in seq_along(db_tl)){
  cat(sprintf("name: %s, %d rows\n", names[i], nrow(db_tl[[i]])))
}

sum(db_tl[['transaction_summary']]$transactions)

## -----------------------------------------------------------------------------
db_summary_dm <- db_summary |>
  rolap::as_dm_class(pk_facts = FALSE)

db_summary_dm |> 
  dm::dm_draw(view_type = "all")

## -----------------------------------------------------------------------------
ct <- rolap::constellation("CSS", list(db_finest, db_summary))

db_tl <- ct |>
  rolap::as_tibble_list()

names <- names(db_tl)
for (i in seq_along(db_tl)){
  cat(sprintf("name: %s, %d rows\n", names[i], nrow(db_tl[[i]])))
}

ct_dm_all <- ct |>
  rolap::as_dm_class(pk_facts = FALSE)

tables <- ct |>
  rolap::get_table_names()

# Degenerate dimension
tables <- base::setdiff(tables, 'transaction')

ct_dm <-
  ct_dm_all[tables]

my_db <- DBI::dbConnect(RSQLite::SQLite())
my_ct_dm <- dm::copy_dm_to(my_db, ct_dm)
my_ct_dm
DBI::dbDisconnect(my_db)

ct_dm |> 
  dm::dm_draw(view_type = "all")

## -----------------------------------------------------------------------------
ct |>
  rolap::get_role_playing_dimension_names()

