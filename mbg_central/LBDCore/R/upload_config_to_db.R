#' @title Uploading config info to SQLite database
#' @description This function will upload the MBG config info into a SQLite database. There are no primary keys set, so any number of duplicate entries can be recorded.
#'
#' @param config The config \code{data.table} prepped apriori.
#' @param user User name
#' @param core_repo Path to core repository
#' @param indicator_group Indicator group
#' @param indicator Indicator
#' @param run_date Run date datestamp
#' @param verbose Print config before exiting?
#'
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite
#'
#' @export
#'
upload_config_to_db <- function(config, user, core_repo, indicator_group, indicator, run_date, verbose = FALSE) {

  ## Prep config file (transpose and add fields)
  config_t <- transpose(config)[2, ]
  colnames(config_t) <- config$V1

  ## Add missing columns
  missing_metadata <- data.table(
    user = user,
    core_repo = core_repo,
    indicator_group = indicator_group,
    indicator = indicator,
    run_date = run_date
  )
  config_binded <- cbind(missing_metadata, config_t)

  ## Add datestamp of adding the config
  config_binded[, datestamp := paste0(Sys.time())]

  ## Open connection to database
  dbpath <- "/share/geospatial/run_logs/config_db/run_db_20190216.sqlite"
  configdb <- DBI::dbConnect(RSQLite::SQLite(), dbpath)

  ## Upload config
  DBI::dbWriteTable(configdb, "config_upload", config_binded, append = TRUE)

  ## Disconnect db
  DBI::dbDisconnect(configdb)

  ## Print config?
  if (verbose) print(config_binded)


  print("Config data uploaded")

  return(NULL)
}
