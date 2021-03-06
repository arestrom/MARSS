
#========================================================
# Get initial set of creel locations for crc areas
#========================================================

# Get all creel sites in selected catch areas for selected years
site_sampler_info = reactive({
  req(valid_connection == TRUE)
  req(input$when_date_range)
  start_date = input$when_date_range[1]
  end_date = input$when_date_range[2]
  site_sampler_info_list = get_site_sampler_info(pool, start_date, end_date) %>%
    select(survey_id, location_id, sampler_id, site_code, creel_site,
           first_name, last_name, sampler_name, survey_date,
           latitude, longitude)
  return(site_sampler_info_list)
})

# Creel site select
output$site_select = renderUI({
  req(site_sampler_info())
  creel_site_list = unique(site_sampler_info()$creel_site)
  pickerInput(inputId = "site_select",
              label = "Select the creel site(s)",
              multiple = TRUE,
              choices = creel_site_list,
              selected = creel_site_list[8],
              width = "100%",
              options = pickerOptions(actionsBox = TRUE))
})

# Get all creel sites in selected catch areas for selected years
creel_site_samplers = reactive({
  req(site_sampler_info())
  req(input$site_select)
  input_site_codes = substr(input$site_select, 1, 4)
  site_sampler_list = site_sampler_info() %>%
    filter(site_code %in% input_site_codes) %>%
    arrange(last_name, first_name) %>%
    select(sampler_id, sampler_name) %>%
    distinct()
  return(site_sampler_list)
})

# Creel site select
output$site_sampler_select = renderUI({
  req(site_sampler_info())
  req(creel_site_samplers())
  pickerInput(inputId = "site_sampler_select",
              label = "Select the sampler(s)",
              multiple = TRUE,
              choices = creel_site_samplers()$sampler_name,
              selected = creel_site_samplers()$sampler_name[2],
              width = "100%",
              options = pickerOptions(actionsBox = TRUE))
})

# Get all creel sites in selected catch areas for selected years
site_sampler_dates = reactive({
  req(site_sampler_info())
  req(creel_site_samplers())
  req(input$site_sampler_select)
  input_site_codes = substr(input$site_select, 1, 4)
  input_sampler_names = input$site_sampler_select
  site_sampler_date_list = site_sampler_info() %>%
    filter(site_code %in% input_site_codes) %>%
    filter(sampler_name %in% input_sampler_names) %>%
    arrange(as.Date(survey_date)) %>%
    mutate(survey_date_dt = format(survey_date, "%m/%d/%Y")) %>%
    mutate(code_date = paste0(site_code, ": ", survey_date_dt))
  return(site_sampler_date_list)
})

# Creel site select
output$site_sampler_date_select = renderUI({
  req(site_sampler_info())
  req(site_sampler_dates())
  site_samp_dt = unique(site_sampler_dates()$code_date)
  pickerInput(inputId = "site_sampler_date_select",
              label = "Select the date(s)",
              multiple = TRUE,
              choices = site_samp_dt,
              selected = site_samp_dt[1],
              width = "100%",
              options = pickerOptions(actionsBox = TRUE))
})

#========================================================
# Get data for initial map
#========================================================

selected_sites = reactive({
  req(site_sampler_dates())
  random_lat = seq(47.03702, 47.03763, by = 0.00001)
  random_lon = seq( -122.89896, -122.89669, by = 0.0001)
  creel_site_coords = site_sampler_dates() %>%
    select(location_id, site_name = creel_site,
           latitude, longitude) %>%
    distinct() %>%
    mutate(site_name = if_else(is.na(latitude) | is.na(longitude),
                               paste0(site_name, " (need coordinates)"),
                               site_name)) %>%
    mutate(latitude = if_else(is.na(latitude),
                              sample(random_lat, size = 1),
                              latitude)) %>%
    mutate(longitude = if_else(is.na(longitude),
                              sample(random_lon, size = 1),
                              longitude))
  return(creel_site_coords)
})

# Sampling location bounds query
site_bounds = reactive({
  req(input$site_select)
  req(selected_sites())
  req(nrow(selected_sites()) > 0L)
  ramp_bounds = selected_sites() %>%
    mutate(min_lat = min(latitude) - 0.0015,
           min_lon = min(longitude) - 0.0015,
           max_lat = max(latitude) + 0.0015,
           max_lon = max(longitude) + 0.0015) %>%
    select(min_lat, min_lon, max_lat, max_lon) %>%
    distinct()
  return(ramp_bounds)
})

# Output leaflet bidn map
output$site_map <- renderLeaflet({
  req(input$site_select)
  req(nrow(selected_sites()) > 0L)
  req(site_bounds())
  validate(
    need(input$site_select != "", "Please select at least one creel site!")
  )
  m = leaflet() %>%
    fitBounds(lng1 = site_bounds()$min_lon,
              lat1 = site_bounds()$min_lat,
              lng2 = site_bounds()$max_lon,
              lat2 = site_bounds()$max_lat) %>%
    addCircleMarkers(
      lng = selected_sites()$longitude,
      lat = selected_sites()$latitude,
      layerId = selected_sites()$location_id,
      popup = selected_sites()$site_name,
      radius = 8,
      color = "red",
      fillOpacity = 0.5,
      stroke = FALSE,
      options = markerOptions(riseOnHover = TRUE)) %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
    addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
    addLayersControl(position = 'bottomleft',
                     baseGroups = c("Esri World Imagery", "Open Topo Map"),
                     options = layersControlOptions(collapsed = TRUE))
  m
})

