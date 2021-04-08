
# Get all creel sites
get_survey_sites = function(pool) {
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
get_survey_samplers = function(pool) {
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
