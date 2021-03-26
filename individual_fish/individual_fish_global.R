
# Main fish_encounter query
get_individual_fish = function(pool, fish_encounter_id) {
  qry = glue("select ind.individual_fish_id, ",
             "flm.fish_length_measurement_id, ",
             "flm.length_measurement_centimeter as length_cm, ",
             "ind.weight_measurement_kilogram as weight_kg, ",
             "sex.sex_description as sex, ",
             "ind.cwt_snout_sample_number as cwt_label, ",
             "chd.cwt_head_taken_status_description as head_taken, ",
             "ind.created_datetime as created_date, ind.created_by, ",
             "ind.modified_datetime as modified_date, ind.modified_by ",
             "from individual_fish as ind ",
             "left join fish_length_measurement as flm on ind.individual_fish_id = flm.individual_fish_id ",
             "left join sex_lut as sex on ind.sex_id = sex.sex_id ",
             "left join cwt_head_taken_status_lut as chd on ",
             "ind.cwt_head_taken_status_id = chd.cwt_head_taken_status_id ",
             "where ind.fish_encounter_id = '{fish_encounter_id}'")
  con = poolCheckout(pool)
  individual_fish = DBI::dbGetQuery(con, qry)
  individual_fish = individual_fish %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(individual_fish_id, fish_length_measurement_id, length_cm, weight_kg,
           sex, cwt_label, head_taken, created_date, created_dt, created_by,
           modified_date, modified_dt, modified_by) %>%
    arrange(created_date)
  poolReturn(con)
  return(individual_fish)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Fish condition
get_sex = function(pool) {
  qry = glue("select sex_id, sex_description as sex ",
             "from sex_lut ",
             "where obsolete_datetime is null")
  con = poolCheckout(pool)
  sex_list = DBI::dbGetQuery(con, qry) %>%
    arrange(sex) %>%
    select(sex_id, sex)
  poolReturn(con)
  return(sex_list)
}

# Fish trauma
get_head_taken = function(pool) {
  qry = glue("select cwt_head_taken_status_id, cwt_head_taken_status_description as head_taken ",
             "from cwt_head_taken_status_lut ",
             "where obsolete_datetime is null")
  con = poolCheckout(pool)
  head_taken_list = DBI::dbGetQuery(con, qry) %>%
    select(cwt_head_taken_status_id, head_taken)
  poolReturn(con)
  return(head_taken_list)
}

#========================================================
# Insert callback
#========================================================

# Define the insert callback
individual_fish_insert = function(pool, new_individual_fish_values) {
  new_insert_values = new_individual_fish_values
  # Generate individual_fish_id since needed in two places
  individual_fish_id = get_uuid(1L)
  # Pull out data for individual_fish
  fish_encounter_id = new_insert_values$fish_encounter_id
  sex_id = new_insert_values$sex_id
  maturity_id = "66cc15ba-1307-4c5d-a7e5-854861d2791a"                        # Unknown...not recorded
  cwt_head_taken_status_id =  new_insert_values$cwt_head_taken_status_id
  fish_color_id = "8a719b86-f3d1-4fe7-b6e2-d2d9e5c846bc"                      # Unknown...not recorded
  fish_processing_status_id = "60876b70-45a7-4887-a54d-6f2b939408e3"          # Not applicable
  sample_category_id = "f21773f3-9b24-4f76-9219-bd875b89e73d"                 # Not applicable
  weight_measurement_kilogram = new_insert_values$weight_kg
  if (is.na(weight_measurement_kilogram) | weight_measurement_kilogram == "") { weight_measurement_kilogram = NA }
  cwt_snout_sample_number = new_insert_values$cwt_label
  if (is.na(cwt_snout_sample_number) | cwt_snout_sample_number == "") { cwt_snout_sample_number = NA }
  created_by = new_insert_values$created_by
  # Pull out data for fish_length_measurement
  fish_length_measurement_type_id = "740dbd1a-93fe-4355-8e78-722afba53b9f"    # Fork length
  length_measurement_centimeter = new_insert_values$length_cm
  #=======================================
  # Insert to individual_fish
  #=======================================
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    insert_ind_result = dbSendStatement(
      con, glue_sql("INSERT INTO individual_fish (",
                    "individual_fish_id, ",
                    "fish_encounter_id, ",
                    "sex_id, ",
                    "maturity_id, ",
                    "cwt_head_taken_status_id, ",
                    "fish_color_id, ",
                    "fish_processing_status_id, ",
                    "sample_category_id, ",
                    "weight_measurement_kilogram, ",
                    "cwt_snout_sample_number, ",
                    "created_by) ",
                    "VALUES (",
                    "$1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)"))
    dbBind(insert_ind_result, list(individual_fish_id,
                                  fish_encounter_id,
                                  sex_id,
                                  maturity_id,
                                  cwt_head_taken_status_id,
                                  fish_color_id,
                                  fish_processing_status_id,
                                  sample_category_id,
                                  weight_measurement_kilogram,
                                  cwt_snout_sample_number,
                                  created_by))
    dbGetRowsAffected(insert_ind_result)
    dbClearResult(insert_ind_result)
    if ( !is.na(length_measurement_centimeter) ) {
      insert_lm_result = dbSendStatement(
        con, glue_sql("INSERT INTO fish_length_measurement (",
                      "individual_fish_id, ",
                      "fish_length_measurement_type_id, ",
                      "length_measurement_centimeter, ",
                      "created_by) ",
                      "VALUES (",
                      "$1, $2, $3, $4)"))
      dbBind(insert_lm_result, list(individual_fish_id,
                                    fish_length_measurement_type_id,
                                    length_measurement_centimeter,
                                    created_by))
      dbGetRowsAffected(insert_lm_result)
      dbClearResult(insert_lm_result)
    }
  })
  poolReturn(con)
}

#========================================================
# Edit update callback
#========================================================

# Define update callback
individual_fish_update = function(pool, individual_fish_edit_values) {
  edit_values = individual_fish_edit_values
  # Pull out data
  individual_fish_id = edit_values$individual_fish_id
  sex_id = edit_values$sex_id
  cwt_head_taken_status_id =  edit_values$cwt_head_taken_status_id
  weight_measurement_kilogram = edit_values$weight_kg
  if (is.na(weight_measurement_kilogram) | weight_measurement_kilogram == "") { weight_measurement_kilogram = NA }
  cwt_snout_sample_number = edit_values$cwt_label
  if (is.na(cwt_snout_sample_number) | cwt_snout_sample_number == "") { cwt_snout_sample_number = NA }
  length_measurement_centimeter = edit_values$length_cm
  if (is.na(length_measurement_centimeter) | length_measurement_centimeter == "") { length_measurement_centimeter = NA }
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  #=======================================
  # Update individual_fish
  #=======================================
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    update_ind_result = dbSendStatement(
      con, glue_sql("UPDATE individual_fish SET ",
                    "sex_id = $1, ",
                    "cwt_head_taken_status_id = $2, ",
                    "weight_measurement_kilogram = $3, ",
                    "cwt_snout_sample_number = $4, ",
                    "modified_datetime = $5, ",
                    "modified_by = $6 ",
                    "where individual_fish_id = $7"))
    dbBind(update_ind_result, list(sex_id,
                                   cwt_head_taken_status_id,
                                   weight_measurement_kilogram,
                                   cwt_snout_sample_number,
                                   mod_dt,
                                   mod_by,
                                   individual_fish_id))
    dbGetRowsAffected(update_ind_result)
    dbClearResult(update_ind_result)
    #=======================================
    # Update fish_length_measurement
    #=======================================
    if ( !is.na(length_measurement_centimeter) ) {
      update_fl_result = dbSendStatement(
        con, glue_sql("UPDATE fish_length_measurement SET ",
                      "length_measurement_centimeter = $1, ",
                      "modified_datetime = $2, ",
                      "modified_by = $3 ",
                      "where individual_fish_id = $4"))
      dbBind(update_fl_result, list(length_measurement_centimeter,
                                    mod_dt,
                                    mod_by,
                                    individual_fish_id))
      dbGetRowsAffected(update_fl_result)
      dbClearResult(update_fl_result)
    }
  })
  poolReturn(con)
}

#========================================================
# Identify individual fish dependencies prior to delete
#========================================================

# Identify fish_encounter dependencies prior to delete
get_individual_fish_dependencies = function(pool, individual_fish_id) {
  qry = glue("select ",
             "count(fm.individual_fish_mark_id) as individual_fish_mark ",
             "from individual_fish_mark as fm ",
             "where fm.individual_fish_id = '{individual_fish_id}'")
  con = poolCheckout(pool)
  individual_fish_dependents = DBI::dbGetQuery(con, qry)
  has_entries = function(x) any(x > 0L)
  individual_fish_dependents = individual_fish_dependents %>%
    select_if(has_entries)
  poolReturn(con)
  return(individual_fish_dependents)
}

#========================================================
# Delete callback
#========================================================

# Define delete callback
individual_fish_delete = function(pool, delete_values) {
  individual_fish_id = delete_values$individual_fish_id
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    #==========================================
    # Delete from fish_length_measurement
    #==========================================
    delete_lm_result = dbSendStatement(
      con, glue_sql("DELETE FROM fish_length_measurement WHERE individual_fish_id = $1"))
    dbBind(delete_lm_result, list(individual_fish_id))
    dbGetRowsAffected(delete_lm_result)
    dbClearResult(delete_lm_result)
    #==========================================
    # Delete from individual_fish
    #==========================================
    delete_ind_result = dbSendStatement(
      con, glue_sql("DELETE FROM individual_fish WHERE individual_fish_id = $1"))
    dbBind(delete_ind_result, list(individual_fish_id))
    dbGetRowsAffected(delete_ind_result)
    dbClearResult(delete_ind_result)
  })
  poolReturn(con)
}











