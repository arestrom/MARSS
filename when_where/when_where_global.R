
# Get list of crc names from drop-down
get_crc_list = function(pool) {
  qry = glue("select loc.location_code as crc_code, loc.location_name as crc_name ",
             "from location as loc ",
             "left join location_type_lut as lt ",
             "on loc.location_type_id = lt.location_type_id ",
             "where lt.location_type_description = 'Catch Record Card area'")
  con = poolCheckout(pool)
  crc = dbGetQuery(con, qry) %>%
    filter(crc_code %in% c("12", "05", "82", "13", "11", "62", "81",
                           "61", "06", "09", "07", "10")) %>%
    mutate(crc_f = factor(crc_code, levels = c("05", "06", "61", "62",
                                               "07", "81", "82", "09",
                                               "10", "11", "12", "13"))) %>%
    arrange(crc_f) %>%
    select(crc_name)
  poolReturn(con)
  return(crc)
}

# Get all creel sites for selected years and catch areas. All sites must have coordinates !!!!
get_year_sites = function(pool, chosen_years, chosen_months) {
  qry = glue("select distinct s.location_id, loc.location_code as site_code, ",
             "st_x(st_transform(lc.geom::geometry, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom::geometry, 4326)) as latitude, ",
             "loc.location_name as site_name, lc.geom as geometry ",
             "from survey as s ",
             "inner join survey_type_lut as st on s.survey_type_id = st.survey_type_id ",
             "inner join location as loc on s.location_id = loc.location_id ",
             "inner join location_coordinates as lc on loc.location_id = lc.location_id ",
             "where date_part('year', survey_datetime) in ({chosen_years}) ",
             "and date_part('month', survey_datetime) in ({chosen_months}) ",
             "and st.survey_type_description in ",
             "('Puget Sound dockside creel survey') ",
             "order by loc.location_name")
  con = poolCheckout(pool)
  sites_st = sf::st_read(con, query = qry, crs = 2927)
  poolReturn(con)
  return(sites_st)
}

# Get polygons for catch areas to enable spatial join
get_catch_polys = function(pool, chosen_catch_areas) {
  qry = glue("select distinct loc.location_name as catch_area, ",
             "lb.geom as geometry ",
             "from location as loc ",
             "inner join location_boundary as lb on loc.location_id = lb.location_id ",
             "where loc.location_name in ({chosen_catch_areas}) ",
             "order by loc.location_name")
  con = poolCheckout(pool)
  areas_st = sf::st_read(con, query = qry, crs = 2927)
  poolReturn(con)
  return(areas_st)
}

# Consolidate to single function
get_creel_sites = function(pool, chosen_years, chosen_months, chosen_catch_areas) {
  sites = get_year_sites(pool, chosen_years, chosen_months)
  polys = get_catch_polys(pool, chosen_catch_areas)
  polys = polys %>%
    st_buffer(., dist = 1000)
  # Combine to get catch_areas
  sites = sites %>%
    st_join(polys) %>%
    st_drop_geometry() %>%
    filter(!is.na(catch_area))
  return(sites)
}

# Get survey dates at chosen sites
get_site_dates = function(pool, chosen_sites, chosen_years, chosen_months) {
  qry = glue("select distinct s.survey_datetime, loc.location_code as site_code, ",
             "loc.location_name ",
             "from survey as s ",
             "inner join survey_type_lut as st on s.survey_type_id = st.survey_type_id ",
             "inner join location as loc on s.location_id = loc.location_id ",
             "inner join location_coordinates as lc on loc.location_id = lc.location_id ",
             "where date_part('year', survey_datetime) in ({chosen_years}) ",
             "and date_part('month', survey_datetime) in ({chosen_months}) ",
             "and st.survey_type_description in ",
             "('Puget Sound dockside creel survey') ",
             "and loc.location_code is not null ",
             "and loc.location_code || ': ' || loc.location_name in ({chosen_sites}) ",
             "order by loc.location_name")
  con = poolCheckout(pool)
  survey_dates = dbGetQuery(con, qry) %>%
    arrange(survey_datetime, site_code) %>%
    mutate(fdate = format(survey_datetime, "%m/%d/%Y")) %>%
    mutate(date_site = paste0(fdate, ": ", site_code)) %>%
    distinct() %>%
    pull(date_site)
  poolReturn(con)
  return(survey_dates)
}

