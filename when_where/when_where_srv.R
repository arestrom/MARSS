
#========================================================
# Get set of years of data to focus on
#========================================================

output$year_select = renderUI({
  year_list = seq(2020, 1986)
  pickerInput("year_select", label = "Select the survey year(s)",
              choices = year_list,
              selected = year_list[9],
              multiple = TRUE,
              width = "100%",
              options = pickerOptions(actionsBox = TRUE))
})

#========================================================
# Get set of months of data to focus on
#========================================================

output$month_select = renderUI({
  req(input$year_select)
  month_list = month.name
  pickerInput("month_select", label = "Select the survey month(s)",
              choices = month_list,
              selected = month_list[7],
              multiple = TRUE,
              width = "100%",
              options = pickerOptions(actionsBox = TRUE))
})

#========================================================
# Get Initial CRC area....set to multiple....shinyWidgets
#========================================================

# CRC select
output$crc_select = renderUI({
  req(input$year_select)
  req(input$month_select)
  req(valid_connection == TRUE)
  crc_list = get_crc_list(pool)$crc_name
  pickerInput(inputId = "crc_select",
              label = "Select Area(s) where the creel site is located",
              multiple = TRUE,
              choices = crc_list,
              selected = "Area 5, Sekiu and Pillar Point",
              width = "100%",
              options = pickerOptions(actionsBox = TRUE))
})

#========================================================
# Get initial set of creel locations for crc areas
#========================================================

# Get all creel sites in selected catch areas for selected years
creel_sites = reactive({
  req(input$year_select)
  req(input$month_select)
  req(input$crc_select)
  chosen_years = input$year_select
  chosen_years = paste0(chosen_years, collapse = ", ")
  chosen_months = input$month_select
  chosen_months = paste0(match(chosen_months, month.name), collapse = ", ")
  chosen_catch_areas = input$crc_select
  chosen_catch_areas = paste0(paste0("'", chosen_catch_areas, "'"), collapse = ", ")
  creel_sites = get_creel_sites(pool, chosen_years, chosen_months, chosen_catch_areas) %>%
    mutate(site_label = paste0(site_code, ": ", site_name)) %>%
    select(location_id, site_code, site_name, site_label, catch_area,
           latitude, longitude)
  return(creel_sites)
})

# Pull out creel_site_list from creel_sites
creel_site_list = reactive({
  creel_site_data = creel_sites() %>%
    select(site_label) %>%
    arrange(site_label) %>%
    distinct() %>%
    pull(site_label)
  return(creel_site_data)
})

# Creel site select
output$site_select = renderUI({
  req(input$year_select)
  req(input$month_select)
  req(input$crc_select)
  pickerInput(inputId = "site_select",
              label = "Select the creel site(s)",
              multiple = TRUE,
              choices = creel_site_list(),
              selected = creel_site_list()[4],
              width = "100%",
              options = pickerOptions(actionsBox = TRUE))
})

#========================================================
# Get available survey dates for years and sites
#========================================================

site_date_list = reactive({
  req(input$year_select)
  req(input$month_select)
  req(input$crc_select)
  req(input$site_select)
  chosen_sites = input$site_select
  chosen_sites = gsub("'", "''", chosen_sites)
  chosen_sites = paste0(paste0("'", chosen_sites, "'"), collapse = ", ")
  chosen_years = input$year_select
  chosen_years = paste0(chosen_years, collapse = ", ")
  chosen_months = input$month_select
  chosen_months = paste0(match(chosen_months, month.name), collapse = ", ")
  survey_dates = get_site_dates(pool, chosen_sites, chosen_years, chosen_months)
  return(survey_dates)
})

# Creel site select
output$date_select = renderUI({
  req(input$year_select)
  req(input$month_select)
  req(input$crc_select)
  req(input$site_select)
  req(!input$site_select == "")
  pickerInput(inputId = "date_select",
              label = "Select the survey date(s)",
              multiple = TRUE,
              choices = site_date_list(),
              selected = site_date_list()[1],
              width = "100%",
              options = pickerOptions(actionsBox = TRUE))
})

#========================================================
# Get data for initial map
#========================================================

selected_sites = reactive({
  req(input$year_select)
  req(input$month_select)
  req(input$crc_select)
  req(input$site_select)
  creel_site_coords = creel_sites() %>%
    filter(site_label %in% input$site_select) %>%
    select(location_id, site_name, latitude, longitude)
  return(creel_site_coords)
})

# Sampling location bounds query
site_bounds = reactive({
  req(input$year_select)
  req(input$month_select)
  req(input$crc_select)
  req(input$site_select)
  req(!input$site_select == "")
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
  req(input$year_select)
  req(input$month_select)
  req(input$crc_select)
  req(input$site_select)
  validate(
    need(input$year_select != "", "Please select at least one year!"),
    need(input$month_select != "", "Please select at least one month!"),
    need(input$crc_select != "", "Please select at least one month!"),
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

