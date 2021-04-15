
# Main sample_site query
get_sample_sites = function(pool) {
  qry = glue("select distinct loc.location_id, lc.location_coordinates_id, ",
             "loc.location_code, loc.location_name, ",
             "loc.location_description, loc.active_indicator as active, ",
             "loc.active_datetime as active_date, ",
             "loc.inactive_datetime as inactive_date, ",
             "loc.inactive_reason_text as inactive_reason, ",
             "loc.comment_text as location_comment, ",
             "st_x(st_transform(lc.geom::geometry, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom::geometry, 4326)) as latitude, ",
             "loc.created_datetime as created_date, loc.created_by, ",
             "loc.modified_datetime as modified_date, loc.modified_by ",
             "from location as loc ",
             "inner join location_type_lut as lt on loc.location_type_id = lt.location_type_id ",
             "left join location_coordinates as lc on loc.location_id = lc.location_id ",
             "where lt.location_type_description = 'Creel survey site' ",
             "order by loc.location_name")
  con = poolCheckout(pool)
  sample_sites = DBI::dbGetQuery(con, qry) %>%
    mutate(latitude = round(latitude, 7)) %>%
    mutate(longitude = round(longitude, 7)) %>%
    mutate(active_date = with_tz(active_date, tzone = "America/Los_Angeles")) %>%
    mutate(active_dt = format(active_date, "%m/%d/%Y")) %>%
    mutate(inactive_date = with_tz(inactive_date, tzone = "America/Los_Angeles")) %>%
    mutate(inactive_dt = format(inactive_date, "%m/%d/%Y")) %>%
    mutate(active = if_else(active == TRUE, "Yes", "No")) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(location_id, location_coordinates_id, location_name,
           location_code, location_description, active,
           active_date, active_dt, inactive_date, inactive_dt,
           inactive_reason, location_comment, latitude, longitude,
           created_date, created_dt, created_by, modified_date,
           modified_dt, modified_by) %>%
    arrange(location_name)
  poolReturn(con)
  return(sample_sites)
}

#========================================================
# Insert callback
#========================================================

# Define the insert callback
sample_site_insert = function(pool, new_sample_site_values) {
  new_insert_values = new_sample_site_values
  # Generate location_id
  location_id = get_uuid(1L)
  created_by = new_insert_values$created_by
  # Pull out location_coordinates table data
  latitude = new_insert_values$latitude
  longitude = new_insert_values$longitude
  # Pull out location table data
  location_name = new_insert_values$location_name
  location_code = new_insert_values$location_code
  location_type_id = "6d561206-4f5f-469b-9a98-5f31883d9e21"  # Creel survey site
  location_description = new_insert_values$location_description
  location_comment = new_insert_values$location_comment
  if (is.na(location_code) | location_code == "") { location_code = NA }
  if (is.na(location_name) | location_name == "") { location_name = NA }
  if (is.na(location_description) | location_description == "") { location_description = NA }
  if (is.na(location_comment) | location_comment == "") { location_comment = NA }
  active_indicator = if_else(new_insert_values$active == "Yes", 1L, 0L)
  active_datetime = new_insert_values$active_datetime
  inactive_datetime = new_insert_values$inactive_datetime
  inactive_reason = new_insert_values$inactive_reason
  if (is.na(inactive_reason) | inactive_reason == "") { inactive_reason = NA }
  # Insert to location table
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    insert_ss_result = dbSendStatement(
      con, glue_sql("INSERT INTO location (",
                    "location_id, ",
                    "location_type_id, ",
                    "location_code, ",
                    "location_name, ",
                    "location_description, ",
                    "active_indicator, ",
                    "active_datetime, ",
                    "inactive_datetime, ",
                    "inactive_reason_text, ",
                    "comment_text, ",
                    "created_by) ",
                    "VALUES (",
                    "$1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)"))
    dbBind(insert_ss_result, list(location_id,
                                  location_type_id,
                                  location_code,
                                  location_name,
                                  location_description,
                                  active_indicator,
                                  active_datetime,
                                  inactive_datetime,
                                  inactive_reason,
                                  location_comment,
                                  created_by))
    dbGetRowsAffected(insert_ss_result)
    dbClearResult(insert_ss_result)
    # Insert coordinates to location_coordinates
    if (!is.na(latitude) & !is.na(longitude) ) {
      # Insert coordinates to location_coordinates
      insert_lc_result = dbSendStatement(
        con, glue_sql("INSERT INTO location_coordinates (",
                      "location_id, ",
                      "geom, ",
                      "created_by) ",
                      "VALUES (",
                      "$1, ",
                      "ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                      "$2)"))
      dbBind(insert_lc_result, list(location_id, created_by))
      dbGetRowsAffected(insert_lc_result)
      dbClearResult(insert_lc_result)
    }
  })
  poolReturn(con)
}

#==============================================================
# Identify sample site surveys prior to update or delete
#==============================================================

# Identify reach_point dependencies prior to delete
get_sample_site_surveys = function(pool, location_id) {
  qry = glue("select s.survey_datetime as survey_date, ",
             "s.start_datetime as start_time, ",
             "s.end_datetime as end_time, ",
             "smp.first_name || ' ' || smp.last_name as sampler_name ",
             "from survey as s ",
             "left join survey_sampler as ss on s.survey_id = ss.survey_id ",
             "left join sampler as smp on ss.sampler_id = smp.sampler_id ",
             "where s.location_id = '{location_id}' ",
             "order by s.survey_datetime desc, s.start_datetime desc ",
             "limit 10")
  con = poolCheckout(pool)
  sample_site_surveys = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  sample_site_surveys = sample_site_surveys %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    mutate(survey_dt = format(survey_date, "%m/%d/%Y")) %>%
    mutate(start_time = with_tz(start_time, tzone = "America/Los_Angeles")) %>%
    mutate(start_time_dt = format(start_time, "%H:%M")) %>%
    mutate(end_time = with_tz(end_time, tzone = "America/Los_Angeles")) %>%
    mutate(end_time_dt = format(end_time, "%H:%M"))
  return(sample_site_surveys)
}

#========================================================
# Edit sample site callback
#========================================================

# Define update callback
sample_site_update = function(pool, sample_site_edit_values, selected_sample_site_data) {
  edit_values = sample_site_edit_values
  # Pull out data for location table
  location_id = edit_values$location_id
  # Pull out location_coordinates table data
  latitude = edit_values$latitude
  longitude = edit_values$longitude
  # Pull out location table data
  location_name = edit_values$location_name
  location_code = edit_values$location_code
  location_description = edit_values$location_description
  location_comment = edit_values$location_comment
  if (is.na(location_code) | location_code == "") { location_code = NA }
  if (is.na(location_name) | location_name == "") { location_name = NA }
  if (is.na(location_description) | location_description == "") { location_description = NA }
  if (is.na(location_comment) | location_comment == "") { location_comment = NA }
  active_indicator = if_else(edit_values$active == "Yes", 1L, 0L)
  active_datetime = edit_values$active_datetime
  inactive_datetime = edit_values$inactive_datetime
  inactive_reason = edit_values$inactive_reason
  if (is.na(inactive_reason) | inactive_reason == "") { inactive_reason = NA }
  mod_dt = format(lubridate::with_tz(Sys.time(), "UTC"))
  mod_by = Sys.getenv("USERNAME")
  created_by = Sys.getenv("USERNAME")
  # Checkout a connection
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    update_result = dbSendStatement(
      con, glue_sql("UPDATE location SET ",
                    "location_code = $1, ",
                    "location_name = $2, ",
                    "location_description = $3, ",
                    "active_indicator = $4, ",
                    "active_datetime = $5, ",
                    "inactive_datetime = $6, ",
                    "inactive_reason_text = $7, ",
                    "comment_text = $8, ",
                    "modified_datetime = $9, ",
                    "modified_by = $10 ",
                    "where location_id = $11"))
    dbBind(update_result, list(location_code,
                               location_name,
                               location_description,
                               active_indicator,
                               active_datetime,
                               inactive_datetime,
                               inactive_reason,
                               location_comment,
                               mod_dt,
                               mod_by,
                               location_id))
    dbGetRowsAffected(update_result)
    dbClearResult(update_result)
    # Insert coordinates to location_coordinates if previous entry does not exist
    if ( is.na(selected_sample_site_data$latitude) & is.na(selected_sample_site_data$longitude) ) {
      if ( !is.na(latitude) & !is.na(longitude) ) {
        # Insert coordinates to location_coordinates
        insert_lc_result = dbSendStatement(
          con, glue_sql("INSERT INTO location_coordinates (",
                        "location_id, ",
                        "geom, ",
                        "created_by) ",
                        "VALUES (",
                        "$1, ",
                        "ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                        "$2)"))
        dbBind(insert_lc_result, list(location_id,
                                      created_by))
        dbGetRowsAffected(insert_lc_result)
        dbClearResult(insert_lc_result)
      }
    # Otherwise update coordinates if previous entry does exist
    } else if (!is.na(selected_sample_site_data$latitude) & !is.na(selected_sample_site_data$longitude) ) {
      if ( !is.na(latitude) & !is.na(longitude) ) {
        update_lc_result = dbSendStatement(
          con, glue_sql("UPDATE location_coordinates SET ",
                        "geom = ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                        "modified_datetime = $1, ",
                        "modified_by = $2 ",
                        "where location_id = $3"))
        dbBind(update_lc_result, list(mod_dt,
                                      mod_by,
                                      location_id))
        dbGetRowsAffected(update_lc_result)
        dbClearResult(update_lc_result)
      } else if ( is.na(latitude) | is.na(longitude) ) {
        dump_coords_result = dbSendStatement(
          con, glue_sql("DELETE FROM location_coordinates ",
                        "WHERE location_id = $1"))
        dbBind(dump_coords_result, list(location_id))
        dbGetRowsAffected(dump_coords_result)
        dbClearResult(dump_coords_result)
      }
    }
  })
  poolReturn(con)
}

#========================================================
# Identify reach_point dependencies prior to delete
#========================================================

# Identify fish_encounter dependencies prior to delete
get_sample_site_dependencies = function(pool, location_id) {
  qry = glue("select ",
             "count(bt.boat_trailer_id) as boat_trailer, ",
             "count(ml.media_location_id) as media_location, ",
             "count(s.survey_id) as survey ",
             "from location as loc ",
             "left join boat_trailer as bt on loc.location_id = bt.trailer_location_id ",
             "left join media_location as ml on loc.location_id = ml.location_id ",
             "left join survey as s on loc.location_id = s.location_id ",
             "where loc.location_id = '{location_id}'")
  con = poolCheckout(pool)
  sample_site_dependents = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  has_entries = function(x) any(x > 0L)
  sample_site_dependents = sample_site_dependents %>%
    select_if(has_entries)
  return(sample_site_dependents)
}

#========================================================
# Delete callback
#========================================================

# Define delete callback
sample_site_delete = function(pool, delete_values) {
  location_id = delete_values$location_id
  # Checkout a connection
  con = poolCheckout(pool)
  delete_result_one = dbSendStatement(
    con, glue_sql("DELETE FROM location_coordinates ",
                  "WHERE location_id = $1"))
  dbBind(delete_result_one, list(location_id))
  dbGetRowsAffected(delete_result_one)
  dbClearResult(delete_result_one)
  delete_result_two = dbSendStatement(
    con, glue_sql("DELETE FROM location ",
                  "WHERE location_id = $1"))
  dbBind(delete_result_two, list(location_id))
  dbGetRowsAffected(delete_result_two)
  dbClearResult(delete_result_two)
  poolReturn(con)
}
