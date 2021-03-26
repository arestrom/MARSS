# Main interview event query
get_interview_event = function(pool, survey_id) {
  qry = glue("select se.survey_event_id, se.encounter_number, ",
             "ca.location_code as catch_area, ",
             "fm.fishing_method_short_description as fishing_method, ",
             "se.uncooperative_angler_indicator as uncooperative_angler, ",
             "se.angler_count, se.created_datetime as created_date, se.created_by, ",
             "se.modified_datetime as modified_date, se.modified_by ",
             "from survey_event as se ",
             "left join location as ca on se.catch_area_id = ca.location_id ",
             "left join fishing_method_lut as fm on se.fishing_method_id = fm.fishing_method_id ",
             "where se.survey_id = '{survey_id}'")
  con = poolCheckout(pool)
  interview_events = DBI::dbGetQuery(con, qry)
  interview_events = interview_events %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    mutate(fish_meth_code = case_when(
      fishing_method == "Not applicable" ~ 0L,
      fishing_method == "Unknown" ~ 0L,
      fishing_method == "Charter diver" ~ 8L,
      fishing_method == "Charter angler" ~ 2L,
      fishing_method == "Pier angler" ~ 3L,
      fishing_method == "Kicker angler" ~ 1L,
      fishing_method == "Kicker diver" ~ 7L,
      fishing_method == "Shore angler" ~ 4L,
      fishing_method == "Shore diver" ~ 5L,
      TRUE ~ 0L)) %>%
    mutate(fishing_method = paste0(fish_meth_code, " ", fishing_method)) %>%
    mutate(cooperative_angler = if_else(uncooperative_angler == TRUE, "No", "Yes")) %>%
    select(survey_event_id, interview_number = encounter_number, catch_area,
           fishing_method, angler_count, cooperative_angler, created_date,
           created_dt, created_by, modified_date, modified_dt, modified_by) %>%
    arrange(interview_number, created_date)
  poolReturn(con)
  return(interview_events)
}

# Main dockside event query
get_dockside_event = function(pool, survey_event_id) {
  qry = glue("select de.dockside_encounter_id, de.survey_event_id, ",
             "de.trip_start_datetime, de.fish_start_datetime, ",
             "de.fish_end_datetime, de.trip_end_datetime, ",
             "de.angler_no_license_count as no_license, ",
             "de.created_datetime as created_date, de.created_by, ",
             "de.modified_datetime as modified_date, de.modified_by ",
             "from dockside_encounter as de ",
             "where de.survey_event_id = '{survey_event_id}'")
  con = poolCheckout(pool)
  dockside_events = DBI::dbGetQuery(con, qry)
  dockside_events = dockside_events %>%
    mutate(trip_start_datetime = with_tz(trip_start_datetime, tzone = "America/Los_Angeles")) %>%
    mutate(trip_start = format(trip_start_datetime, "%m/%d/%Y %H:%M")) %>%
    mutate(fish_start_datetime = with_tz(fish_start_datetime, tzone = "America/Los_Angeles")) %>%
    mutate(fish_start = format(fish_start_datetime, "%m/%d/%Y %H:%M")) %>%
    mutate(fish_end_datetime = with_tz(fish_end_datetime, tzone = "America/Los_Angeles")) %>%
    mutate(fish_end = format(fish_end_datetime, "%m/%d/%Y %H:%M")) %>%
    mutate(trip_end_datetime = with_tz(trip_end_datetime, tzone = "America/Los_Angeles")) %>%
    mutate(trip_end = format(trip_end_datetime, "%m/%d/%Y %H:%M")) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(dockside_encounter_id, survey_event_id, trip_start_datetime, trip_start,
           fish_start_datetime, fish_start, fish_end_datetime, fish_end, trip_end_datetime,
           trip_end, no_license, created_date, created_dt, created_by, modified_date,
           modified_dt, modified_by) %>%
    arrange(created_date)
  poolReturn(con)
  return(dockside_events)
}

# Main target event query
get_target_event = function(pool, survey_event_id) {
  qry = glue("select ts.target_species_id, ts.survey_event_id, ",
             "tt.target_species_type_code as target_species, ",
             "ts.created_datetime as created_date, ",
             "ts.created_by, ts.modified_datetime as modified_date, ",
             "ts.modified_by ",
             "from target_species as ts ",
             "left join target_species_type_lut as tt on ",
             "ts.target_species_type_id = tt.target_species_type_id ",
             "where ts.survey_event_id = '{survey_event_id}'")
  con = poolCheckout(pool)
  target_events = DBI::dbGetQuery(con, qry)
  target_events = target_events %>%
    mutate(target_code = case_when(
      target_species == "Smelt" ~ 2L,
      target_species == "Chinook" ~ 1L,
      target_species == "Steelhead" ~ 8L,
      target_species == "Rockfish" ~ 14L,
      target_species == "Salmonid" ~ 1L,
      target_species == "Not applicable" ~ 0L,
      target_species == "Unknown" ~ 5L,
      target_species == "Sturgeon" ~ 2L,
      target_species == "Cutthroat" ~ 9L,
      target_species == "Lingcod" ~ 10L,
      target_species == "Coho" ~ 1L,
      target_species == "Marine" ~ 2L,
      target_species == "Shad" ~ 2L,
      target_species == "Walleye" ~ 0L,
      target_species == "Halibut" ~ 4L,
      target_species == "Dungeness Crab" ~ 0L,
      TRUE ~ 0L)) %>%
    arrange(target_species) %>%
    mutate(target_species = paste0(target_code, " ", target_species)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(target_species_id, survey_event_id, target_species, created_date,
           created_dt, created_by, modified_date, modified_dt, modified_by) %>%
    arrange(created_date)
  poolReturn(con)
  return(target_events)
}

#==========================================================================
# Get generic lut input values...data_source, etc.
#==========================================================================

# Get list of crc names from drop-down
get_catch_area = function(pool) {
  qry = glue("select loc.location_id, loc.location_code as catch_area ",
             "from location as loc ",
             "left join location_type_lut as lt ",
             "on loc.location_type_id = lt.location_type_id ",
             "where lt.location_type_description in ",
             "('Catch Record Card area', 'Special Area Fishery', 'Not applicable') ",
             "and not loc.location_code is NULL")
  con = poolCheckout(pool)
  crc = dbGetQuery(con, qry) %>%
    filter(catch_area %in% c('05', '06', '61', '62', '07', '81', '82', '09',
                             '10', '11', '12', '12N', '12S', '13', '15',
                             '16', '17', '19', '20', '23', '26', '28', '29',
                             '30', '31', '33', '34', '01', '02', '21', '22',
                             '03', '04', '4B', '99', '83D', '850')) %>%
    mutate(crc_f = factor(catch_area, levels = c('05', '06', '61', '62', '07', '81', '82', '09',
                                                 '10', '11', '12', '12N', '12S', '13', '15',
                                                 '16', '17', '19', '20', '23', '26', '28', '29',
                                                 '30', '31', '33', '34', '01', '02', '21', '22',
                                                 '03', '04', '4B', '99', '83D', '850'))) %>%
    arrange(crc_f) %>%
    select(location_id, catch_area)
  poolReturn(con)
  return(crc)
}

# Get list of crc names from drop-down
get_fishing_method = function(pool) {
  qry = glue("select fishing_method_id, fishing_method_short_description as fishing_method ",
             "from fishing_method_lut")
  con = poolCheckout(pool)
  fish_meth = dbGetQuery(con, qry) %>%
    mutate(fish_meth_code = case_when(
      fishing_method == "Not applicable" ~ 0L,
      fishing_method == "Unknown" ~ 0L,
      fishing_method == "Charter diver" ~ 8L,
      fishing_method == "Charter angler" ~ 2L,
      fishing_method == "Pier angler" ~ 3L,
      fishing_method == "Kicker angler" ~ 1L,
      fishing_method == "Kicker diver" ~ 7L,
      fishing_method == "Shore angler" ~ 4L,
      fishing_method == "Shore diver" ~ 5L,
      TRUE ~ 0L)) %>%
    mutate(fishing_method = paste0(fish_meth_code, " ", fishing_method)) %>%
    arrange(fishing_method) %>%
    select(fishing_method_id, fishing_method)
  poolReturn(con)
  return(fish_meth)
}

# Get list of crc names from drop-down
get_target_species = function(pool) {
  qry = glue("select target_species_type_id, target_species_type_code as target_species ",
             "from target_species_type_lut")
  con = poolCheckout(pool)
  target_sp = dbGetQuery(con, qry) %>%
    mutate(target_code = case_when(
      target_species == "Smelt" ~ 2L,
      target_species == "Chinook" ~ 1L,
      target_species == "Steelhead" ~ 8L,
      target_species == "Rockfish" ~ 14L,
      target_species == "Salmonid" ~ 1L,
      target_species == "Not applicable" ~ 0L,
      target_species == "Unknown" ~ 5L,
      target_species == "Sturgeon" ~ 2L,
      target_species == "Cutthroat" ~ 9L,
      target_species == "Lingcod" ~ 10L,
      target_species == "Coho" ~ 1L,
      target_species == "Marine" ~ 2L,
      target_species == "Shad" ~ 2L,
      target_species == "Walleye" ~ 0L,
      target_species == "Halibut" ~ 4L,
      target_species == "Dungeness Crab" ~ 0L,
      TRUE ~ 0L)) %>%
    arrange(target_species) %>%
    mutate(target_species = paste0(target_code, " ", target_species)) %>%
    select(target_species_type_id, target_species)
  poolReturn(con)
  return(target_sp)
}

#========================================================
# Interview event insert callback
#========================================================

# Define the insert callback
interview_event_insert = function(pool, new_interview_values) {
  new_interview_values = new_interview_values
  # Pull out data for survey_event
  survey_id = new_interview_values$survey_id
  survey_event_id = get_uuid(1L)
  catch_area_id = new_interview_values$catch_area_id
  encounter_location_id = "eb9f298d-7a52-4bc3-83d2-071d48b6ddce"   # Not applicable
  fishing_method_id = new_interview_values$fishing_method_id
  encounter_number = new_interview_values$interview_number
  angler_count = new_interview_values$angler_count
  uncooperative_angler_indicator = new_interview_values$uncooperative_angler_indicator
  uncooperative_angler_indicator = if_else(uncooperative_angler_indicator == "Yes", 1L, 0L)
  incomplete_trip_indicator = 0L          # FALSE
  duplicate_encounter_indicator = 0L      # FALSE
  void_encounter_indicator = 0L           # FALSE
  created_by = new_interview_values$created_by
  #=======================================
  # Insert to survey_event table
  #=======================================
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    insert_se_result = dbSendStatement(
      con, glue_sql("INSERT INTO survey_event (",
                    "survey_event_id, ",
                    "survey_id, ",
                    "catch_area_id, ",
                    "encounter_location_id, ",
                    "fishing_method_id, ",
                    "encounter_number, ",
                    "angler_count, ",
                    "uncooperative_angler_indicator, ",
                    "incomplete_trip_indicator, ",
                    "duplicate_encounter_indicator, ",
                    "void_encounter_indicator, ",
                    "created_by) ",
                    "VALUES (",
                    "$1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)"))
    dbBind(insert_se_result, list(survey_event_id,
                                  survey_id,
                                  catch_area_id,
                                  encounter_location_id,
                                  fishing_method_id,
                                  encounter_number,
                                  angler_count,
                                  uncooperative_angler_indicator,
                                  incomplete_trip_indicator,
                                  duplicate_encounter_indicator,
                                  void_encounter_indicator,
                                  created_by))
    dbGetRowsAffected(insert_se_result)
    dbClearResult(insert_se_result)
  })
  poolReturn(con)
}

#========================================================
# Interview edit update callback
#========================================================

# Define update callback
interview_event_update = function( pool, interview_event_edit_values ) {
  edit_values = interview_event_edit_values
  # Pull out data
  survey_event_id = edit_values$survey_event_id
  catch_area_id = edit_values$catch_area_id
  fishing_method_id = edit_values$fishing_method_id
  encounter_number = edit_values$interview_number
  angler_count = edit_values$angler_count
  uncooperative_angler_indicator = edit_values$cooperative_angler
  uncooperative_angler_indicator = if_else(uncooperative_angler_indicator == "Yes", 1L, 0L)
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    update_result = dbSendStatement(
      con, glue_sql("UPDATE survey_event SET ",
                    "catch_area_id = $1, ",
                    "fishing_method_id = $2, ",
                    "encounter_number = $3, ",
                    "angler_count = $4, ",
                    "uncooperative_angler_indicator = $5, ",
                    "modified_datetime = $6, ",
                    "modified_by = $7 ",
                    "where survey_event_id = $8"))
    dbBind(update_result, list(catch_area_id,
                               fishing_method_id,
                               encounter_number,
                               angler_count,
                               uncooperative_angler_indicator,
                               mod_dt,
                               mod_by,
                               survey_event_id))
    dbGetRowsAffected(update_result)
    dbClearResult(update_result)
  })
  poolReturn(con)
}

#========================================================
# Interview delete callback
#========================================================

# Identify interview event dependencies prior to delete.
get_interview_event_dependencies = function(pool, survey_event_id) {
  qry = glue("select ",
             "count(be.bird_encounter_id) as bird_encounter, ",
             "count(de.dockside_encounter_id) as dockside_encounter, ",
             "count(fe.fish_encounter_id) as fish_encounter, ",
             "count(ts.target_species_id) as target_species ",
             "from survey_event as se ",
             "left join bird_encounter as be on se.survey_event_id = be.survey_event_id ",
             "left join dockside_encounter as de on se.survey_event_id = de.survey_event_id ",
             "left join fish_encounter as fe on se.survey_event_id = fe.survey_event_id ",
             "left join target_species as ts on se.survey_event_id = ts.survey_event_id ",
             "where se.survey_event_id = '{survey_event_id}'")
  con = poolCheckout(pool)
  interview_event_dependents = DBI::dbGetQuery(con, qry)
  has_entries = function(x) any(x > 0L)
  interview_event_dependents = interview_event_dependents %>%
    select_if(has_entries)
  poolReturn(con)
  return(interview_event_dependents)
}

# Define delete callback
interview_event_delete = function(pool, interview_delete_values) {
  survey_event_id = interview_delete_values$survey_event_id
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    delete_int_result = dbSendStatement(
      con, glue_sql("DELETE FROM survey_event WHERE survey_event_id = $1"))
    dbBind(delete_int_result, list(survey_event_id))
    dbGetRowsAffected(delete_int_result)
    dbClearResult(delete_int_result)
  })
  poolReturn(con)
}

#========================================================
# Dockside event insert callback
#========================================================

# Define the insert callback
dockside_event_insert = function(pool, new_dockside_event_values) {
  new_dockside_event_values = new_dockside_event_values
  # Pull out data for dockside_encounter
  survey_event_id = new_dockside_event_values$survey_event_id
  fish_start_datetime = new_dockside_event_values$fish_start
  fish_end_datetime = new_dockside_event_values$fish_end
  trip_start_datetime = new_dockside_event_values$trip_start
  trip_end_datetime = new_dockside_event_values$trip_end
  angler_no_license_count = new_dockside_event_values$no_license
  out_of_sample_indicator = 0L
  created_by = new_dockside_event_values$created_by
  #=======================================
  # Insert to dockside_encounter
  #=======================================
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    insert_de_result = dbSendStatement(
      con, glue_sql("INSERT INTO dockside_encounter (",
                    "survey_event_id, ",
                    "fish_start_datetime, ",
                    "fish_end_datetime, ",
                    "trip_start_datetime, ",
                    "trip_end_datetime, ",
                    "angler_no_license_count, ",
                    "out_of_sample_indicator, ",
                    "created_by) ",
                    "VALUES (",
                    "$1, $2, $3, $4, $5, $6, $7, $8)"))
    dbBind(insert_de_result, list(survey_event_id,
                                  fish_start_datetime,
                                  fish_end_datetime,
                                  trip_start_datetime,
                                  trip_end_datetime,
                                  angler_no_license_count,
                                  out_of_sample_indicator,
                                  created_by))
    dbGetRowsAffected(insert_de_result)
    dbClearResult(insert_de_result)
  })
  poolReturn(con)
}

#========================================================
# Dockside event update callback
#========================================================

# Define update callback
dockside_event_update = function( pool, dockside_event_edit_values ) {
  edit_values = dockside_event_edit_values
  # Pull out data
  dockside_encounter_id = edit_values$dockside_encounter_id
  fish_start_datetime = edit_values$fish_start
  fish_end_datetime = edit_values$fish_end
  trip_start_datetime = edit_values$trip_start
  trip_end_datetime = edit_values$trip_end
  angler_no_license_count = edit_values$no_license
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    update_result = dbSendStatement(
      con, glue_sql("UPDATE dockside_encounter SET ",
                    "fish_start_datetime = $1, ",
                    "fish_end_datetime = $2, ",
                    "trip_start_datetime = $3, ",
                    "trip_end_datetime = $4, ",
                    "angler_no_license_count = $5, ",
                    "modified_datetime = $6, ",
                    "modified_by = $7 ",
                    "where dockside_encounter_id = $8"))
    dbBind(update_result, list(fish_start_datetime,
                               fish_end_datetime,
                               trip_start_datetime,
                               trip_end_datetime,
                               angler_no_license_count,
                               mod_dt,
                               mod_by,
                               dockside_encounter_id))
    dbGetRowsAffected(update_result)
    dbClearResult(update_result)
  })
  poolReturn(con)
}

#========================================================
# Dockside delete callback
#========================================================

# Define delete callback
dockside_event_delete = function(pool, dockside_delete_values) {
  dockside_encounter_id = dockside_delete_values$dockside_encounter_id
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    delete_dock_result = dbSendStatement(
      con, glue_sql("DELETE FROM dockside_encounter WHERE dockside_encounter_id = $1"))
    dbBind(delete_dock_result, list(dockside_encounter_id))
    dbGetRowsAffected(delete_dock_result)
    dbClearResult(delete_dock_result)
  })
  poolReturn(con)
}

#========================================================
# Target event insert callback
#========================================================

# Define the insert callback
target_event_insert = function(pool, new_target_event_values) {
  new_target_event_values = new_target_event_values
  # Pull out data for survey_event
  survey_event_id = new_target_event_values$survey_event_id
  # Pull out data for target_species
  target_species_type_id = new_target_event_values$target_species_type_id
  created_by = new_target_event_values$created_by
  #=======================================
  # Insert to target_species table
  #=======================================
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    insert_ts_result = dbSendStatement(
      con, glue_sql("INSERT INTO target_species (",
                    "survey_event_id, ",
                    "target_species_type_id, ",
                    "created_by) ",
                    "VALUES (",
                    "$1, $2, $3)"))
    dbBind(insert_ts_result, list(survey_event_id,
                                  target_species_type_id,
                                  created_by))
    dbGetRowsAffected(insert_ts_result)
    dbClearResult(insert_ts_result)
  })
  poolReturn(con)
}

#========================================================
# Target event update callback
#========================================================

# Define the insert callback
target_event_update = function(pool, target_event_edit_values) {
  edit_values = target_event_edit_values
  # Pull out data for survey_event
  target_species_id = edit_values$target_species_id
  # Pull out data for target_species
  target_species_type_id = edit_values$target_species_type_id
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  #=======================================
  # Insert to target_species table
  #=======================================
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    update_result = dbSendStatement(
      con, glue_sql("UPDATE target_species SET ",
                    "target_species_type_id = $1, ",
                    "modified_datetime = $2, ",
                    "modified_by = $3 ",
                    "where target_species_id = $4"))
    dbBind(update_result, list(target_species_type_id,
                               mod_dt,
                               mod_by,
                               target_species_id))
    dbGetRowsAffected(update_result)
    dbClearResult(update_result)
  })
  poolReturn(con)
}

#========================================================
# Target event delete callback
#========================================================

# Define delete callback
target_event_delete = function(pool, target_delete_values) {
  target_species_id = target_delete_values$target_species_id
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    delete_target_result = dbSendStatement(
      con, glue_sql("DELETE FROM target_species WHERE target_species_id = $1"))
    dbBind(delete_target_result, list(target_species_id))
    dbGetRowsAffected(delete_target_result)
    dbClearResult(delete_target_result)
  })
  poolReturn(con)
}












