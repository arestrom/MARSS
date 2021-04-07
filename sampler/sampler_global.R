
# Get sampler names currently in the database
get_samplers = function(pool) {
  qry = glue("select sampler_id, first_name, last_name, ",
             "first_name || ' ' || last_name as full_name, ",
             "active_indicator, created_datetime, created_by, ",
             "modified_datetime, modified_by ",
             "from sampler")
  con = poolCheckout(pool)
  samplers = DBI::dbGetQuery(con, qry) %>%
    mutate(active = if_else(active_indicator == TRUE, "Yes", "No")) %>%
    mutate(created_date = with_tz(created_datetime, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_datetime, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(sampler_id, first_name, last_name, full_name, active, created_date,
           created_dt, created_by, modified_date, modified_dt, modified_by) %>%
    arrange(last_name, first_name)
  poolReturn(con)
  return(samplers)
}

#========================================================
# Insert callback
#========================================================

# Define the insert callback for new companions
sampler_insert = function(pool, new_sampler_values) {
  new_sampler_values = new_sampler_values
  # Pull out data
  first_name = new_sampler_values$first_name
  last_name = new_sampler_values$last_name
  active = new_sampler_values$active
  active_indicator = if_else(active == "Yes", 1L, 0L)
  created_by = new_sampler_values$created_by
  # Checkout a connection
  con = poolCheckout(pool)
  insert_result = dbSendStatement(
    con, glue_sql("INSERT INTO sampler (",
                  "first_name, ",
                  "last_name, ",
                  "active_indicator, ",
                  "created_by ) ",
                  "VALUES (",
                  "$1, $2, $3, $4)"))
  dbBind(insert_result, list(first_name, last_name,
                             active_indicator, created_by))
  dbGetRowsAffected(insert_result)
  dbClearResult(insert_result)
  poolReturn(con)
}

#========================================================
# Edit update callback
#========================================================

# Define update callback
sampler_update = function(pool, sampler_edit_values) {
  edit_values = sampler_edit_values
  # Pull out data
  sampler_id = edit_values$sampler_id
  first_name = edit_values$first_name
  last_name = edit_values$last_name
  active = edit_values$active
  active_indicator = if_else(active == "Yes", 1L, 0L)
  mod_dt = format(lubridate::with_tz(Sys.time(), "UTC"))
  mod_by = Sys.getenv("USERNAME")
  # Checkout a connection
  con = poolCheckout(pool)
  update_result = dbSendStatement(
    con, glue_sql("UPDATE sampler SET ",
                  "first_name = $1, ",
                  "last_name = $2, ",
                  "active_indicator = $3, ",
                  "modified_datetime = $4, ",
                  "modified_by = $5 ",
                  "where sampler_id = $6"))
  dbBind(update_result, list(first_name,
                             last_name,
                             active_indicator,
                             mod_dt,
                             mod_by,
                             sampler_id))
  dbGetRowsAffected(update_result)
  dbClearResult(update_result)
  poolReturn(con)
}

#========================================================
# Delete callback
#========================================================

# Identify sampler dependencies prior to delete
get_sampler_dependencies = function(pool, sampler_id) {
  qry = glue("select ",
             "count(ss.sampler_id) as survey_sampler ",
             "from survey_sampler as ss ",
             "where ss.sampler_id = '{sampler_id}'")
  con = poolCheckout(pool)
  sampler_dependents = DBI::dbGetQuery(pool, qry)
  poolReturn(con)
  has_entries = function(x) any(x > 0L)
  sampler_dependents = sampler_dependents %>%
    select_if(has_entries)
  return(sampler_dependents)
}

# Define delete callback for sampler
sampler_delete = function(pool, delete_values) {
  sampler_id = delete_values$sampler_id
  con = poolCheckout(pool)
  delete_result = dbSendStatement(
    con, glue_sql("DELETE FROM sampler WHERE sampler_id = $1"))
  dbBind(delete_result, list(sampler_id))
  dbGetRowsAffected(delete_result)
  dbClearResult(delete_result)
  poolReturn(con)
}
