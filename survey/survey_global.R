# Function to get header data...use single-select for year
get_surveys = function(pool, survey_ids) {
  qry = glue("select s.survey_id, s.survey_datetime as survey_date, loc.location_id, ",
             "loc.location_code || ': ' || loc.location_name as survey_site, ",
             "smp.sampler_id, smp.first_name || ' ' || smp.last_name as sampler_name, ",
             "s.start_datetime as start_time, ",
             "s.end_datetime as end_time, ",
             "sdt.survey_design_type_description as survey_design, ",
             "s.no_effort_indicator as no_effort, ",
             "s.comment_text as survey_comment, ",
             "s.created_datetime as created_date, ",
             "s.created_by, s.modified_datetime as modified_date, ",
             "s.modified_by ",
             "from survey as s ",
             "inner join location as loc on s.location_id = loc.location_id ",
             "left join survey_sampler as ss on s.survey_id = ss.survey_id ",
             "left join sampler as smp on ss.sampler_id = smp.sampler_id ",
             "left join survey_design_type_lut as sdt ",
             "on s.survey_design_type_id = sdt.survey_design_type_id ",
             "where s.survey_id in ({survey_ids})")
  con = poolCheckout(pool)
  surveys = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  surveys = surveys %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    mutate(survey_date_dt = format(survey_date, "%m/%d/%Y")) %>%
    mutate(start_time = with_tz(start_time, tzone = "America/Los_Angeles")) %>%
    mutate(start_time_dt = format(start_time, "%H:%M")) %>%
    mutate(end_time = with_tz(end_time, tzone = "America/Los_Angeles")) %>%
    mutate(end_time_dt = format(end_time, "%H:%M")) %>%
    mutate(any_effort = if_else(no_effort == TRUE, "No", "Yes")) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    mutate(survey_date = as.Date(survey_date)) %>%
    select(survey_id, survey_date, survey_date_dt, location_id, survey_site,
           sampler_id, sampler_name, start_time, start_time_dt, end_time, end_time_dt,
           survey_design, any_effort, survey_comment, created_date, created_dt,
           created_by, modified_date, modified_dt, modified_by) %>%
    arrange(survey_date, start_time, end_time, created_date)
  return(surveys)
}

#==========================================================================
# Get generic lut input values...data_source, etc.
#==========================================================================

# Get all creel sites
get_all_sites = function(pool) {
  qry = glue("select distinct loc.location_id, loc.location_name, ",
             "loc.location_code || ': ' || loc.location_name as survey_site ",
             "from location as loc ",
             "inner join location_type_lut ",
             "as lt on loc.location_type_id = lt.location_type_id ",
             "where lt.location_type_description = 'Creel survey site' ",
             "order by loc.location_name")
  con = poolCheckout(pool)
  site_list = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  return(site_list)
}

# Samplers
get_samplers = function(pool) {
  qry = glue("select sampler_id, first_name, last_name, ",
             "first_name || ' ' || last_name as sampler_name ",
             "from sampler ",
             "order by last_name, first_name")
  con = poolCheckout(pool)
  sampler_list = DBI::dbGetQuery(con, qry) %>%
    select(sampler_id, sampler_name)
  poolReturn(con)
  return(sampler_list)
}

# Survey design
get_survey_design = function(pool) {
  qry = glue("select survey_design_type_id, survey_design_type_description as survey_design ",
             "from survey_design_type_lut")
  con = poolCheckout(pool)
  survey_design_list = DBI::dbGetQuery(con, qry) %>%
    arrange(survey_design) %>%
    select(survey_design_type_id, survey_design)
  poolReturn(con)
  return(survey_design_list)
}


#==========================================================================
# Validate survey create operations
#==========================================================================

# Check for existing surveys prior to survey insert operation
dup_survey = function(new_survey_vals, existing_survey_vals) {
  matching_rows = new_survey_vals %>%
    inner_join(existing_survey_vals,
               by = c("survey_date", "survey_site", "sampler_name", "start_time",
                      "end_time", "any_effort"))
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
survey_insert = function(pool, new_values) {
  # Hard-coded values
  survey_id = get_uuid(1L)                                                  # Dual table entry
  survey_type_id = "a1c68f6d-dcfe-4b86-a00d-d8fab7057e2e"                   # PS dockside creel
  sampling_program_id = "1908cc7d-ef42-4345-9285-8961096c8613"              # PSSP
  data_review_status_id = "59cbf9d7-9015-49b6-874d-ea37ef663c56"            # Reviewed
  survey_completion_status_id = "d192b32e-0e4f-4719-9c9c-dec6593b1977"      # Completed
  # Pull out data
  survey_design_type_id = new_values$survey_design_id
  location_id = new_values$survey_site_id
  survey_datetime = new_values$survey_datetime
  start_datetime = new_values$start_datetime
  end_datetime = new_values$end_datetime
  no_effort_indicator = new_values$no_effort_indicator
  comment_text = new_values$survey_comment
  created_by = new_values$created_by
  sampler_id = new_values$sampler_id

  # Insert new survey
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    survey_insert_result = dbSendStatement(
      con, glue_sql("INSERT INTO survey (",
                    "survey_id, ",
                    "survey_type_id, ",
                    "survey_design_type_id, ",
                    "sampling_program_id, ",
                    "location_id, ",
                    "data_review_status_id, ",
                    "survey_completion_status_id, ",
                    "survey_datetime, ",
                    "start_datetime, ",
                    "end_datetime, ",
                    "no_effort_indicator, ",
                    "comment_text, ",
                    "created_by) ",
                    "VALUES (",
                    "$1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)"))
    dbBind(survey_insert_result, list(survey_id, survey_type_id, survey_design_type_id,
                                      sampling_program_id, location_id, data_review_status_id,
                                      survey_completion_status_id, survey_datetime,
                                      start_datetime, end_datetime, no_effort_indicator,
                                      comment_text, created_by))
    dbGetRowsAffected(survey_insert_result)
    dbClearResult(survey_insert_result)

    # Insert new survey_sampler entry
    ss_insert_result = dbSendStatement(
      con, glue_sql("INSERT INTO survey_sampler (",
                    "survey_id, ",
                    "sampler_id) ",
                    "VALUES (",
                    "$1, $2)"))
    dbBind(ss_insert_result, list(survey_id, sampler_id))
    dbGetRowsAffected(ss_insert_result)
    dbClearResult(ss_insert_result)
  })
  poolReturn(con)
}

#========================================================
# Edit update callback
#========================================================

# Define update callback
survey_update = function(pool, edit_values) {
  # Pull out data
  survey_design_type_id = edit_values$survey_design_type_id
  location_id = edit_values$location_id
  survey_datetime = edit_values$survey_datetime
  start_datetime = edit_values$start_datetime
  end_datetime = edit_values$end_datetime
  no_effort_indicator = edit_values$no_effort_indicator
  comment_text = edit_values$survey_comment
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  sampler_id = edit_values$sampler_id
  survey_id = edit_values$survey_id
  # Connect
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    # Update survey_sampler table
    ss_update_result = dbSendStatement(
      con, glue_sql("UPDATE survey_sampler SET ",
                    "sampler_id = $1 ",
                    "where survey_id = $2"))
    dbBind(ss_update_result, list(sampler_id, survey_id))
    dbGetRowsAffected(ss_update_result)
    dbClearResult(ss_update_result)
    # Update survey table
    survey_update_result = dbSendStatement(
      con, glue_sql("UPDATE survey SET ",
                    "survey_design_type_id = $1, ",
                    "location_id = $2, ",
                    "survey_datetime = $3, ",
                    "start_datetime = $4, ",
                    "end_datetime = $5, ",
                    "no_effort_indicator = $6, ",
                    "comment_text = $7, ",
                    "modified_datetime = $8, ",
                    "modified_by = $9 ",
                    "where survey_id = $10"))
    dbBind(survey_update_result, list(survey_design_type_id, location_id, survey_datetime,
                                      start_datetime, end_datetime, no_effort_indicator,
                                      comment_text, mod_dt, mod_by, survey_id))
    dbGetRowsAffected(survey_update_result)
    dbClearResult(survey_update_result)
  })
  poolReturn(con)
}

#========================================================
# Identify dependencies prior to delete
#========================================================

# Identify survey dependencies prior to delete....do the same for survey_event
get_survey_dependencies = function(pool, survey_id) {
  qry = glue("select count(bc.boat_count_omitted_id) as boat_count_omitted, ",
             "count(bt.boat_trailer_id) as boat_trailer, ",
             "count(ms.mobile_survey_form_id) as mobile_survey_form, ",
             "count(rl.released_legal_fish_id) as released_legal_fish, ",
             "count(se.survey_event_id) as survey_event, ",
             "count(sm.survey_mobile_device_id) as survey_mobile_device ",
             "from survey as s ",
             "left join boat_count_omitted as bc on s.survey_id = bc.survey_id ",
             "left join boat_trailer as bt on s.survey_id = bt.survey_id ",
             "left join mobile_survey_form as ms on s.survey_id = ms.survey_id ",
             "left join released_legal_fish as rl on s.survey_id = rl.survey_id ",
             "left join survey_event as se on s.survey_id = se.survey_id ",
             "left join survey_mobile_device as sm on s.survey_id = sm.survey_id ",
             "where s.survey_id = '{survey_id}'")
  con = poolCheckout(pool)
  survey_dependents = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  has_entries = function(x) any(x > 0L)
  survey_dependents = survey_dependents %>%
    select_if(has_entries)
  return(survey_dependents)
}

#========================================================
# Delete callback
#========================================================

# Define delete callback
survey_delete = function(pool, delete_values) {
  survey_id = delete_values$survey_id
  con = poolCheckout(pool)
  DBI::dbWithTransaction(con, {
    # Delete from survey_sampler
    ss_delete_result = dbSendStatement(
      con, glue_sql("DELETE FROM survey_sampler WHERE survey_id = $1"))
    dbBind(ss_delete_result, list(survey_id))
    dbGetRowsAffected(ss_delete_result)
    dbClearResult(ss_delete_result)
    # Delete from survey
    survey_delete_result = dbSendStatement(
      con, glue_sql("DELETE FROM survey WHERE survey_id = $1"))
    dbBind(survey_delete_result, list(survey_id))
    dbGetRowsAffected(survey_delete_result)
    dbClearResult(survey_delete_result)
  })
  poolReturn(con)
}

