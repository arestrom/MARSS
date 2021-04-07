
# Get sampler names currently in the database
get_samplers = function(pool) {
  qry = glue("select sampler_id, first_name, last_name, ",
             "first_name || ' ' || last_name as full_name, ",
             "active_indicator, created_datetime, created_by, ",
             "modified_datetime, modified_by ",
             "from sampler")
  samplers = DBI::dbGetQuery(pool, qry) %>%
    mutate(active = if_else(active_indicator == TRUE, "Yes", "No")) %>%
    mutate(created_date = as.POSIXct(created_datetime, tz = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = as.POSIXct(modified_datetime, tz = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(sampler_id, first_name, last_name, full_name, active, created_date,
           created_dt, created_by, modified_date, modified_dt, modified_by) %>%
    arrange(last_name, first_name)
  return(samplers)
}
