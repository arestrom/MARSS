
# Get all creel sites
get_survey_query_sites = function(pool) {
  qry = glue("select distinct loc.location_id, loc.location_name, ",
             "loc.location_code || ': ' || loc.location_name as survey_site ",
             "from location as loc ",
             "inner join location_type_lut ",
             "as lt on loc.location_type_id = lt.location_type_id ",
             "where lt.location_type_description = 'Creel survey site' ",
             "order by loc.location_name")
  con = poolCheckout(pool)
  survey_site_list = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  return(survey_site_list)
}

# Samplers
get_survey_query_samplers = function(pool) {
  qry = glue("select sampler_id, first_name, last_name, ",
             "first_name || ' ' || last_name as sampler_name ",
             "from sampler ",
             "order by last_name, first_name")
  con = poolCheckout(pool)
  survey_sampler_list = DBI::dbGetQuery(con, qry) %>%
    select(sampler_id, sampler_name)
  poolReturn(con)
  return(survey_sampler_list)
}

# Get all creel sites for selected years and catch areas
get_query_surveys = function(pool, start_date, end_date, sites, samplers) {
  qry = glue("select s.survey_id, s.survey_datetime as survey_date, loc.location_name, ",
             "loc.location_code || ': ' || loc.location_name as survey_site, ",
             "smp.last_name, smp.first_name, ",
             "smp.first_name || ' ' || smp.last_name as sampler_name, ",
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
             "inner join location_type_lut as lt on loc.location_type_id = lt.location_type_id ",
             "inner join survey_sampler as ss on s.survey_id = ss.survey_id ",
             "inner join sampler as smp on ss.sampler_id = smp.sampler_id ",
             "left join survey_type_lut as st on s.survey_type_id = st.survey_type_id ",
             "left join survey_design_type_lut as sdt on s.survey_design_type_id = sdt.survey_design_type_id ",
             "where date(s.survey_datetime::timestamp at time zone 'America/Los_Angeles') >= '{start_date}' ",
             "and date(s.survey_datetime::timestamp at time zone 'America/Los_Angeles') <= '{end_date}' ",
             "and st.survey_type_description in ('Puget Sound dockside creel survey') ",
             "and lt.location_type_description = 'Creel survey site' ",
             "and s.location_id in ({sites}) and ss.sampler_id in ({samplers}) ",
             "order by loc.location_name, s.survey_datetime, smp.last_name, smp.first_name")
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
    select(survey_id, survey_date, survey_date_dt, survey_site,
           sampler_name, start_time, start_time_dt, end_time, end_time_dt,
           survey_design, any_effort, survey_comment, created_date, created_dt,
           created_by, modified_date, modified_dt, modified_by) %>%
    arrange(survey_date, start_time, end_time, created_date)
  return(surveys)
}

# Get core interview info
get_query_event = function(pool, survey_id) {
  qry = glue("select se.survey_event_id, se.encounter_number, ",
             "ca.location_code as catch_area, ",
             "fm.fishing_method_short_description as fishing_method, ",
             "se.uncooperative_angler_indicator as uncooperative_angler, ",
             "se.angler_count, de.trip_start_datetime, de.fish_start_datetime, ",
             "de.fish_end_datetime, de.trip_end_datetime, ",
             "se.created_datetime as created_date, se.created_by, ",
             "se.modified_datetime as modified_date, se.modified_by ",
             "from survey_event as se ",
             "left join location as ca on se.catch_area_id = ca.location_id ",
             "left join fishing_method_lut as fm on se.fishing_method_id = fm.fishing_method_id ",
             "left join dockside_encounter as de on se.survey_event_id = de.survey_event_id ",
             "where se.survey_id = '{survey_id}'")
  con = poolCheckout(pool)
  interview_events = DBI::dbGetQuery(con, qry)
  interview_events = interview_events %>%
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
           fishing_method, angler_count, cooperative_angler, trip_start,
           fish_start, fish_end, trip_end, created_dt, created_by,
           modified_dt, modified_by) %>%
    arrange(interview_number, fish_start)
  poolReturn(con)
  return(interview_events)
}

