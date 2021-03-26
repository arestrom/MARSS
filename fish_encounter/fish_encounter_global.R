# Main survey event query
get_fish_encounter = function(pool, survey_event_id) {
  qry = glue("select fe.fish_encounter_id, fe.survey_event_id, ",
             "sp.species_id, sp.species_code || ' ' || sp.common_name as species,",
             "fe.fish_count, ad.adipose_clip_status_code as ad_mark, ",
             "cr.catch_result_type_description as catch_result, ",
             "ed.encounter_depth_range_code as depth_range, ",
             "ed.encounter_depth_range as range, ",
             "ed.obsolete_flag as range_flag, ",
             "eg.encounter_gear_type_code || ': ' || gear_type_description as gear_type, ",
             "fe.created_datetime as created_date, fe.created_by, ",
             "fe.modified_datetime as modified_date, fe.modified_by ",
             "from fish_encounter as fe ",
             "left join species_lut as sp on fe.species_id = sp.species_id ",
             "left join adipose_clip_status_lut as ad on fe.adipose_clip_status_id = ad.adipose_clip_status_id ",
             "left join catch_result_type_lut as cr on fe.catch_result_type_id = cr.catch_result_type_id ",
             "left join encounter_depth_range_lut as ed on fe.encounter_depth_range_id = ed.encounter_depth_range_id ",
             "left join encounter_gear_type_lut as eg on fe.encounter_gear_type_id = eg.encounter_gear_type_id ",
             "where fe.survey_event_id = '{survey_event_id}'")
  con = poolCheckout(pool)
  fish_events = DBI::dbGetQuery(con, qry)
  fish_events = fish_events %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    mutate(range = if_else(range_flag == TRUE, paste0(range, " ", "(pre-2010)"), range)) %>%
    mutate(depth_range = paste0(depth_range, ": ", range)) %>%
    select(fish_encounter_id, survey_event_id, species_id, species, fish_count,
           ad_mark, catch_result, depth_range, gear_type, created_date, created_dt,
           created_by, modified_date, modified_dt, modified_by) %>%
    arrange(created_date, fish_encounter_id)
  poolReturn(con)
  return(fish_events)
}

#==========================================================================
# Get generic lut input values...data_source, etc.
#==========================================================================

# Species
get_event_species = function(pool) {
  qry = glue("select species_id as event_species_id, ",
             "species_code || ' ' || common_name as event_species ",
             "from species_lut ",
             "where obsolete_datetime is null ",
             "and not species_code = '' ",
             "and species_code is not null ",
             "order by species_code")
  con = poolCheckout(pool)
  event_species_list = DBI::dbGetQuery(con, qry) %>%
    select(event_species_id, event_species)
  poolReturn(con)
  return(event_species_list)
}

# Catch result
get_catch_result = function(pool) {
  qry = glue("select catch_result_type_id, catch_result_type_description as catch_result ",
             "from catch_result_type_lut ",
             "where obsolete_datetime is null")
  con = poolCheckout(pool)
  catch_result_list = DBI::dbGetQuery(con, qry) %>%
    select(catch_result_type_id, catch_result)
  poolReturn(con)
  return(catch_result_list)
}

# AD mark
get_ad_mark = function(pool) {
  qry = glue("select adipose_clip_status_id, adipose_clip_status_code as ad_mark ",
             "from adipose_clip_status_lut ",
             "where obsolete_datetime is null")
  con = poolCheckout(pool)
  ad_mark_list = DBI::dbGetQuery(con, qry) %>%
    mutate(mark_f = factor(ad_mark, levels = c("UM", "AD", "UD", "UN", "NC", "na"))) %>%
    arrange(mark_f) %>%
    select(adipose_clip_status_id, ad_mark)
  poolReturn(con)
  return(ad_mark_list)
}

# Depth range
get_depth_range = function(pool) {
  qry = glue("select encounter_depth_range_id, encounter_depth_range_code as depth_range, ",
             "encounter_depth_range as range, obsolete_flag ",
             "from encounter_depth_range_lut")
  con = poolCheckout(pool)
  depth_range_list = DBI::dbGetQuery(con, qry) %>%
    arrange(depth_range) %>%
    mutate(range = if_else(obsolete_flag == TRUE, paste0(range, " ", "(pre-2010)"), range)) %>%
    mutate(depth_range = paste0(depth_range, ": ", range)) %>%
    select(encounter_depth_range_id, depth_range)
  poolReturn(con)
  return(depth_range_list)
}

# Gear type
get_gear_type = function(pool) {
  qry = glue("select encounter_gear_type_id, ",
             "encounter_gear_type_code || ': ' || gear_type_description as gear_type ",
             "from encounter_gear_type_lut ",
             "where obsolete_datetime is null")
  con = poolCheckout(pool)
  gear_type_list = DBI::dbGetQuery(con, qry) %>%
    select(encounter_gear_type_id, gear_type)
  poolReturn(con)
  return(gear_type_list)
}

#==========================================================================
# Validate survey_event insert operations
#==========================================================================

# Check for existing duplicate survey_event prior to survey_event insert operation
dup_fish_event = function(new_fish_event_vals, existing_fish_event_vals) {
  new_fish_event_vals = new_fish_event_vals %>%
    mutate(species = if_else(species == "", NA_character_, species)) %>%
    mutate(catch_result = if_else(catch_result == "", NA_character_, catch_result)) %>%
    mutate(ad_mark = if_else(ad_mark == "", NA_character_, ad_mark)) %>%
    mutate(depth_range = if_else(depth_range == "", NA_character_, depth_range)) %>%
    mutate(depth_range = if_else(depth_range == "NA: NA", NA_character_, depth_range)) %>%
    mutate(gear_type = if_else(gear_type == "", NA_character_, gear_type)) %>%
    mutate(fish_count = as.integer(fish_count)) %>%
    select(species, fish_count, catch_result, ad_mark, depth_range, gear_type)
  existing_fish_event_vals = existing_fish_event_vals %>%
    mutate(fish_count = as.integer(fish_count)) %>%
    mutate(depth_range = if_else(depth_range == "", NA_character_, depth_range)) %>%
    mutate(depth_range = if_else(depth_range == "NA: NA", NA_character_, depth_range))
  matching_rows = new_fish_event_vals %>%
    inner_join(existing_fish_event_vals,
               by = c( "species", "fish_count", "catch_result", "ad_mark", "depth_range",
                      "gear_type"))
  if (nrow(matching_rows) > 0 ) {
    dup_flag = TRUE
  } else {
    dup_flag = FALSE
  }
  return(dup_flag)
}

#========================================================
# Insert callback
#========================================================

# Define the insert callback
fish_event_insert = function(pool, new_fish_event_values) {
  new_event_values = new_fish_event_values
  survey_event_id = new_event_values$survey_event_id
  # Pull out data for fish_encounter
  species_id = new_event_values$species_id
  adipose_clip_status_id = new_event_values$adipose_clip_status_id
  encounter_gear_type_id = new_event_values$encounter_gear_type_id
  encounter_depth_range_id = new_event_values$encounter_depth_range_id
  catch_result_type_id = new_event_values$catch_result_type_id
  fish_count = new_event_values$fish_count
  created_by = new_event_values$created_by
  if ( !is.na(species_id) ) {
    # Generate some default values in case any required fields are missing
    adipose_clip_status_id = if_else(is.na(adipose_clip_status_id),
                                     "1d61246c-003b-49e9-b2a3-cffdddb3905c",     # Not applicable
                                     adipose_clip_status_id)
    encounter_gear_type_id = if_else(is.na(encounter_gear_type_id),
                                     "b896b52b-7a27-493e-800f-5f0badbdbdca",     # Unknown
                                     encounter_gear_type_id)
    encounter_depth_range_id = if_else(is.na(encounter_depth_range_id),
                                       "cf60f146-0d1f-47d1-b6cf-58d5ab3161cc",   # Not applicable
                                       encounter_depth_range_id)
    tag_disposition_id = "c3cccb4e-16bd-4b0a-b42c-0b3de28f4706"                  # Not applicable
    legal_size_status_id = "52d88b33-6ca9-40da-8019-31e6b722ccfa"                # Not applicable
    if ( is.na(catch_result_type_id) | catch_result_type_id == "468a853c-668a-4978-9fa2-ba9dee873247" ) {   # Kept
      release_health_type_id = "fc09db3d-929d-490f-9e9f-f6a0f42af0e5"            # Not applicable
    } else {
      release_health_type_id = "3517da9a-45e4-433a-8ec1-64a818ba48cb"            # No data
    }
    catch_result_type_id = if_else(is.na(catch_result_type_id),
                                   "3d18830c-8b33-40f7-b2a5-edb1359b3867",       # Not applicable
                                   catch_result_type_id)
    fish_count = if_else(is.na(fish_count), 1L, fish_count)
  }
  #=======================================
  # Insert to fish_encounter
  #=======================================
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    if ( !is.na(species_id) ) {
      insert_fe_result = dbSendStatement(
        con, glue_sql("INSERT INTO fish_encounter (",
                      "survey_event_id, ",
                      "species_id, ",
                      "adipose_clip_status_id, ",
                      "encounter_gear_type_id, ",
                      "encounter_depth_range_id, ",
                      "tag_disposition_id, ",
                      "legal_size_status_id, ",
                      "catch_result_type_id, ",
                      "release_health_type_id, ",
                      "fish_count, ",
                      "created_by) ",
                      "VALUES (",
                      "$1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)"))
      dbBind(insert_fe_result, list(survey_event_id,
                                    species_id,
                                    adipose_clip_status_id,
                                    encounter_gear_type_id,
                                    encounter_depth_range_id,
                                    tag_disposition_id,
                                    legal_size_status_id,
                                    catch_result_type_id,
                                    release_health_type_id,
                                    fish_count,
                                    created_by))
      dbGetRowsAffected(insert_fe_result)
      dbClearResult(insert_fe_result)
    }
  })
  poolReturn(con)
}

#========================================================
# Edit update callback
#========================================================

# Define update callback
fish_event_update = function(pool, fish_event_edit_values) {
  edit_values = fish_event_edit_values
  # Pull out data for fish_encounter
  fish_encounter_id = edit_values$fish_encounter_id
  species_id = edit_values$species_id
  adipose_clip_status_id = edit_values$adipose_clip_status_id
  encounter_gear_type_id = edit_values$encounter_gear_type_id
  encounter_depth_range_id = edit_values$encounter_depth_range_id
  catch_result_type_id = edit_values$catch_result_type_id
  fish_count = edit_values$fish_count
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  #=======================================
  # Update fish_encounter
  #=======================================
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    update_fe_result = dbSendStatement(
      con, glue_sql("UPDATE fish_encounter SET ",
                    "species_id = $1, ",
                    "adipose_clip_status_id = $2, ",
                    "encounter_gear_type_id = $3, ",
                    "encounter_depth_range_id = $4, ",
                    "catch_result_type_id = $5, ",
                    "fish_count = $6, ",
                    "modified_datetime = $7, ",
                    "modified_by = $8 ",
                    "where fish_encounter_id = $9"))
    dbBind(update_fe_result, list(species_id,
                                  adipose_clip_status_id,
                                  encounter_gear_type_id,
                                  encounter_depth_range_id,
                                  catch_result_type_id,
                                  fish_count,
                                  mod_dt,
                                  mod_by,
                                  fish_encounter_id))
    dbGetRowsAffected(update_fe_result)
    dbClearResult(update_fe_result)
  })
  poolReturn(con)
}

#========================================================
# Identify species dependencies prior to delete
#========================================================

# Identify fish encounter dependencies prior to delete
get_fish_event_dependencies = function(pool, fish_encounter_id) {
  qry = glue("select ",
             "count(ind.individual_fish_id) as individual_fish ",
             "from individual_fish as ind ",
             "where ind.fish_encounter_id = '{fish_encounter_id}'")
  con = poolCheckout(pool)
  fish_event_dependents = DBI::dbGetQuery(con, qry)
  has_entries = function(x) any(x > 0L)
  fish_event_dependents = fish_event_dependents %>%
    select_if(has_entries)
  poolReturn(con)
  return(fish_event_dependents)
}

#========================================================
# Delete callback
#========================================================

# Define delete callback
fish_event_delete = function(pool, delete_values) {
  fish_encounter_id = delete_values$fish_encounter_id
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    delete_fe_result = dbSendStatement(
      con, glue_sql("DELETE FROM fish_encounter WHERE fish_encounter_id = $1"))
    dbBind(delete_fe_result, list(fish_encounter_id))
    dbGetRowsAffected(delete_fe_result)
    dbClearResult(delete_fe_result)
  })
  poolReturn(con)
}



