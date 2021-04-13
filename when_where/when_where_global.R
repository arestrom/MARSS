
# Get all creel sites for selected years and catch areas
get_site_sampler_info = function(pool, start_date, end_date) {
  qry = glue("select distinct s.location_id, loc.location_code as site_code, ",
             "loc.location_name, s.survey_id, smp.sampler_id, smp.first_name, ",
             "smp.last_name, smp.first_name || ' ' || smp.last_name as sampler_name, ",
             "loc.location_code || ': ' || loc.location_name as creel_site, ",
             "s.survey_datetime as survey_date, ",
             "st_x(st_transform(lc.geom::geometry, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom::geometry, 4326)) as latitude ",
             "from survey as s ",
             "inner join survey_type_lut as st on s.survey_type_id = st.survey_type_id ",
             "inner join location as loc on s.location_id = loc.location_id ",
             "inner join location_type_lut as lt on loc.location_type_id = lt.location_type_id ",
             "left join location_coordinates as lc on loc.location_id = lc.location_id ",
             "inner join survey_sampler as ss on s.survey_id = ss.survey_id ",
             "inner join sampler as smp on ss.sampler_id = smp.sampler_id ",
             "where s.survey_datetime >= '{start_date}' ",
             "and s.survey_datetime <= '{end_date}' ",
             "and st.survey_type_description in ",
             "('Puget Sound dockside creel survey') ",
             "and lt.location_type_description = 'Creel survey site' ",
             "order by loc.location_name")
  con = poolCheckout(pool)
  site_sampler_info_list = DBI::dbGetQuery(con, qry) %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    select(survey_id, location_id, sampler_id, site_code, creel_site,
           first_name, last_name, sampler_name, survey_date,
           latitude, longitude)
  poolReturn(con)
  return(site_sampler_info_list)
}
