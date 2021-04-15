
#========================================================
# Generate lut select ui's
#========================================================

output$active_select = renderUI({
  active_list = c("Yes", "No")
  selectizeInput("active_select", label = "Active?",
                 choices = active_list, selected = "Yes",
                 width = "75px")
})

#========================================================
# Primary datatable for sample site
#========================================================

# Primary DT datatable
output$sample_sites = renderDT({
  req(input$tabs == "sample_site")
  req(valid_connection == TRUE)
  sample_site_title = glue("Sample sites")
  sample_site_data = get_sample_sites(pool) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_dt, inactive_dt,
           inactive_reason, location_description,
           location_comment, created_dt, created_by,
           modified_dt, modified_by)

  # Generate table
  datatable(sample_site_data,
            colnames = c("Site name", "Site code", "Latitude", "Longitude",
                         "Active?", "Active date", "Inactive date",
                         "Inactive reason", "Site description", "Comment",
                         "Create DT", "Create By", "Mod DT", "Mod By"),
            selection = list(mode = 'single'),
            options = list(dom = 'ltp',
                           pageLength = 10,
                           lengthMenu = c(10, 25, 50, 100, 500),
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(sample_site_title))))
})

# Create surveys DT proxy object
sample_site_dt_proxy = dataTableProxy(outputId = "sample_sites")

#========================================================
# Collect location values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_sample_site_data = reactive({
  req(input$tabs == "sample_site")
  req(input$sample_sites_rows_selected)
  sample_site_data = get_sample_sites(pool)
  sample_site_row = input$sample_sites_rows_selected
  selected_sample_site = tibble(location_id = sample_site_data$location_id[sample_site_row],
                                location_coordinates_id = sample_site_data$location_coordinates_id[sample_site_row],
                                location_name = sample_site_data$location_name[sample_site_row],
                                location_code = sample_site_data$location_code[sample_site_row],
                                latitude = sample_site_data$latitude[sample_site_row],
                                longitude = sample_site_data$longitude[sample_site_row],
                                active = sample_site_data$active[sample_site_row],
                                active_date = sample_site_data$active_date[sample_site_row],
                                inactive_date = sample_site_data$inactive_date[sample_site_row],
                                inactive_reason = sample_site_data$inactive_reason[sample_site_row],
                                location_description = sample_site_data$location_description[sample_site_row],
                                location_comment = sample_site_data$location_comment[sample_site_row],
                                created_date = sample_site_data$created_date[sample_site_row],
                                created_by = sample_site_data$created_by[sample_site_row],
                                modified_date = sample_site_data$modified_date[sample_site_row],
                                modified_by = sample_site_data$modified_by[sample_site_row])
  return(selected_sample_site)
})

#========================================================
# Update inputs to values in selected row
#========================================================

# Update all input values to values in selected row
observeEvent(input$sample_sites_rows_selected, {
  sssdat = selected_sample_site_data()
  # Account for cases where time values are missing
  if (is.na(sssdat$active_date) ) {
    active_date = NULL
  } else {
    active_date = sssdat$active_date
  }
  if (is.na(sssdat$inactive_date) ) {
    inactive_date = NULL
  } else {
    inactive_date = sssdat$inactive_date
  }
  updateTextInput(session, "location_name_input", value = sssdat$location_name)
  updateTextInput(session, "location_code_input", value = sssdat$location_code)
  updateNumericInput(session, "sample_site_latitude_input", value = sssdat$latitude)
  updateNumericInput(session, "sample_site_longitude_input", value = sssdat$longitude)
  updateSelectizeInput(session, "active_select", selected = sssdat$active)
  updateAirDateInput(session, "active_date_input", value = active_date, clear = TRUE)
  updateAirDateInput(session, "inactive_date_input", value = inactive_date, clear = TRUE)
  updateTextAreaInput(session, "inactive_reason_input", value = sssdat$inactive_reason)
  updateTextAreaInput(session, "sample_site_description_input", value = sssdat$location_description)
  updateTextAreaInput(session, "sample_site_comment_input", value = sssdat$location_comment)
})

#================================================================
# Get either selected site coordinates or default centroid
#================================================================

# Output leaflet bidn map....could also use color to indicate species:
# Notes:
# 1. input$reach_point_rows_selected is either row number or NULL
# 2. input$reach_point_latitude_input is either value or NA
# See: https://rstudio.github.io/leaflet/markers.html
output$sample_site_map <- renderLeaflet({
  req(input$tabs == "sample_site")
  req(valid_connection == TRUE)
  # Force map to rerender every time use_reach_point_map button is clicked
  input$use_sample_site_map
  site_coords = get_sample_sites(pool) %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    mutate(min_lat = min(latitude),
           min_lon = min(longitude),
           max_lat = max(latitude),
           max_lon = max(longitude)) %>%
    mutate(site_label = paste0(location_name, " (", location_code, ")")) %>%
    select(location_id, site_label, latitude, longitude,
           min_lat, min_lon, max_lat, max_lon)
  # Get data for setting map bounds ========================
  if ( is.na(input$sample_site_latitude_input) |
       is.na(input$sample_site_longitude_input) ) {
    bounds = tibble(lng1 = -124,
                    lat1 = 47,
                    lng2 = -121.5,
                    lat2 = 49)
  } else {
    # Add buffer, otherwise, if only a single point is available in reach_coords,
    # map zoom may be so high that the map does not render
    bounds = tibble(lng1 = (input$sample_site_longitude_input - 0.0015),
                    lat1 = (input$sample_site_latitude_input - 0.0015),
                    lng2 = (input$sample_site_longitude_input + 0.0015),
                    lat2 = (input$sample_site_latitude_input + 0.0015))
  }
  # Generate basemap ======================================
  edit_site_loc = leaflet() %>%
    fitBounds(lng1 = bounds$lng1,
              lat1 = bounds$lat1,
              lng2 = bounds$lng2,
              lat2 = bounds$lat2) %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
    addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
    addLayersControl(position = 'bottomright',
                     baseGroups = c("Esri World Imagery", "Open Topo Map"),
                     overlayGroups = c("Sites"),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    leaflet.extras::addSearchOSM(.) %>%
    # Add edit features
    leaflet.extras::addDrawToolbar(.,
                                   targetGroup = "site_edits",
                                   position = "topleft",
                                   polylineOptions = FALSE,
                                   polygonOptions = FALSE,
                                   circleOptions = FALSE,
                                   rectangleOptions = FALSE,
                                   markerOptions = FALSE,
                                   circleMarkerOptions = drawCircleMarkerOptions(
                                     color = "#ace600",
                                     stroke = TRUE,
                                     weight = 2,
                                     fillOpacity = 0.5),
                                   editOptions = editToolbarOptions(
                                     remove = FALSE,
                                     selectedPathOptions = selectedPathOptions()),
                                   singleFeature = TRUE)
  # Test if reach_coords has data ==========================
  if ( !nrow(site_coords) > 0L ) {
    return(edit_site_loc)
  } else {
    edit_site_loc_two = edit_site_loc %>%
      # Add existing data if locations exist ===================
    addCircleMarkers(
      lng = site_coords$longitude,
      lat = site_coords$latitude,
      popup = site_coords$site_label,
      layerId = site_coords$location_id,
      radius = 8,
      color = "red",
      fillOpacity = 0.5,
      stroke = FALSE,
      options = markerOptions(draggable = FALSE,
                              riseOnHover = TRUE))
    return(edit_site_loc_two)
  }
})

# Create a reactive values object for drawn_features
site_edit_rv = reactiveValues(lat = NULL, lon = NULL)

# Set rv to NULL on initiation of map
observeEvent(input$use_sample_site_map, {
  site_edit_rv$lat = NULL
  site_edit_rv$lon = NULL
}, priority = 9999)

# Assign coordinates from mapedit circle marker
observeEvent(c(input$sample_site_map_draw_all_features), {
  site_edit_rv$lat = as.numeric(input$sample_site_map_draw_all_features$features[[1]]$geometry$coordinates[[2]])
  site_edit_rv$lon = as.numeric(input$sample_site_map_draw_all_features$features[[1]]$geometry$coordinates[[1]])
})

# Get html output of updated locations
output$sample_site_coordinates = renderUI({
  coords_out = HTML(glue("Sample site location: ", {site_edit_rv$lat}, ": ", {site_edit_rv$lon}))
  return(coords_out)
})

# Modal for new reach points...add or edit a point...write coordinates to lat, lon
observeEvent(input$use_sample_site_map, {
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "sample_site_map_modal",
             modalDialog (
               size = 'l',
               title = glue("Add or edit a sample site"),
               fluidPage (
                 fluidRow(
                   column(width = 12,
                          leafletOutput("sample_site_map", height = 500) %>%
                            shinycssloaders::withSpinner(., size = 0.5),
                          br()
                   )
                 ),
                 fluidRow(
                   column(width = 3,
                          actionButton("capture_sample_site", "Save coordinates"),
                          tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
                                tooltip = glue("<span style='font-size:11px;'>",
                                               "You can zoom in on the map and use the circle marker ",
                                               "tool at the upper left to place a marker where the ",
                                               "sample site should be located. You can use the ",
                                               "edit tool to fine-tune the position of the marker. ",
                                               "When done, click on the 'Save coordinates' button.<span>"))),
                   column(width = 9,
                          htmlOutput("sample_site_coordinates"))
                 )
               ),
               easyClose = TRUE,
               footer = NULL
             )
    )
  )
}, priority = -1)

#======================================================================
# Update coordinate inputs to coordinates selected on map
#======================================================================

# Update all input values to values in selected row
observeEvent(input$capture_sample_site, {
  updateNumericInput(session, "sample_site_latitude_input", value = site_edit_rv$lat)
  updateNumericInput(session, "sample_site_longitude_input", value = site_edit_rv$lon)
  removeModal()
}, priority = 9999)

# Set rv to NULL
observeEvent(input$capture_sample_site, {
  site_edit_rv$lat = NULL
  site_edit_rv$lon = NULL
}, priority = -1)

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
sample_site_create = reactive({
  # Date values
  active_date = format(input$active_date_input)
  if (nchar(active_date) < 10) { active_date = NA_character_ }
  active_date = as.POSIXct(active_date)
  inactive_date = format(input$inactive_date_input)
  if (nchar(inactive_date) < 10) { inactive_date = NA_character_ }
  inactive_date = as.POSIXct(active_date)
  new_sample_site = tibble(location_name = input$location_name_input,
                           location_code = input$location_code_input,
                           latitude = input$sample_site_latitude_input,
                           longitude = input$sample_site_longitude_input,
                           active = input$active_select,
                           active_date = active_date,
                           inactive_date = inactive_date,
                           inactive_reason = input$inactive_reason_input,
                           location_description = input$sample_site_description_input,
                           location_comment = input$sample_site_comment_input,
                           created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                           created_by = Sys.getenv("USERNAME"))
  new_sample_site = new_sample_site %>%
    mutate(active = if_else(is.na(active) | active == "",
                                        "No", active)) %>%
    mutate(inactive_reason = trimws(inactive_reason)) %>%
    mutate(inactive_reason = if_else(is.na(inactive_reason) | inactive_reason == "",
                                    NA_character_, inactive_reason)) %>%
    mutate(location_description = trimws(location_description)) %>%
    mutate(location_description = if_else(is.na(location_description) | location_description == "",
                                     NA_character_, location_description)) %>%
    mutate(location_comment = trimws(location_comment)) %>%
    mutate(location_comment = if_else(is.na(location_comment) | location_comment == "",
                                          NA_character_, location_comment))
  return(new_sample_site)
})

# Generate values to show in modal
output$sample_site_modal_insert_vals = renderDT({
  sample_site_modal_in_vals = sample_site_create() %>%
    mutate(active_date = format(active_date, "%m/%d/%Y")) %>%
    mutate(inactive_date = format(inactive_date, "%m/%d/%Y")) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_date,
           inactive_date, inactive_reason,
           location_description, location_comment)
  # Generate table
  datatable(sample_site_modal_in_vals,
            colnames = c("Site name", "Site code", "Latitude", "Longitude",
                         "Active?", "Active date", "Inactive date",
                         "Inactive reason", "Site description", "Comment"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new reach point locations
observeEvent(input$sample_site_add, {
  new_sample_site_vals = sample_site_create()
  old_sample_site_vals = get_sample_sites(pool)
  new_coords = paste0(new_sample_site_vals$latitude, ":", new_sample_site_vals$longitude)
  old_coords = paste0(old_sample_site_vals$latitude, ":", old_sample_site_vals$longitude)
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "sample_site_insert_modal",
             if ( is.na(new_sample_site_vals$location_name) |
                  is.na(new_sample_site_vals$location_code) |
                  is.na(new_sample_site_vals$latitude) |
                  is.na(new_sample_site_vals$longitude) |
                  is.na(new_sample_site_vals$active) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required for all fields highlighted in green"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Verify location name has not already been used
             } else if ( new_sample_site_vals$location_name %in% old_sample_site_vals$location_name ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("This sample site name has already been entered"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Verify location code has not already been used
             } else if ( new_sample_site_vals$location_code %in% old_sample_site_vals$location_code ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("This sample site code has already been entered"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Verify no set of coordinates are duplicated
             } else if ( new_coords %in% old_coords ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("This set of coordinates has already been entered"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               # Write to DB
               modalDialog (
                 size = 'l',
                 title = glue("Insert new sample site to the database?"),
                 fluidPage (
                   DT::DTOutput("sample_site_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_sample_site", "Insert sample site")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
sample_site_insert_vals = reactive({
  new_sample_site_values = sample_site_create() %>%
    mutate(active_datetime = if_else(!is.na(active_date),
                                     with_tz(as.POSIXct(format(active_date),
                                                        tz = "America/Los_Angeles"), tzone = "UTC"),
                                     active_date)) %>%
    mutate(inactive_datetime = if_else(!is.na(inactive_date),
                                       with_tz(as.POSIXct(format(inactive_date),
                                                          tz = "America/Los_Angeles"), tzone = "UTC"),
                                       inactive_date)) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_datetime,
           inactive_datetime, inactive_reason,
           location_description, location_comment,
           created_by)
  return(new_sample_site_values)
})

# Update DB and reload DT
observeEvent(input$insert_sample_site, {
  tryCatch({
    sample_site_insert(pool, sample_site_insert_vals())
    shinytoastr::toastr_success("New sample_site was entered")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_sample_site_insert_vals = get_sample_sites(pool) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_dt, inactive_dt,
           inactive_reason, location_description,
           location_comment, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(sample_site_dt_proxy, post_sample_site_insert_vals)
}, priority = 9999)

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
sample_site_edit = reactive({
  # Date values
  active_date = format(input$active_date_input)
  if (nchar(active_date) < 10) { active_date = NA_character_ }
  active_date = as.POSIXct(active_date)
  inactive_date = format(input$inactive_date_input)
  if (nchar(inactive_date) < 10) { inactive_date = NA_character_ }
  inactive_date = as.POSIXct(inactive_date)
  edit_sample_site = tibble(location_id = selected_sample_site_data()$location_id,
                            location_name = input$location_name_input,
                            location_code = input$location_code_input,
                            latitude = input$sample_site_latitude_input,
                            longitude = input$sample_site_longitude_input,
                            active = input$active_select,
                            active_date = active_date,
                            inactive_date = inactive_date,
                            inactive_reason = input$inactive_reason_input,
                            location_description = input$sample_site_description_input,
                            location_comment = input$sample_site_comment_input,
                            modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
                            modified_by = Sys.getenv("USERNAME"))
  edit_sample_site = edit_sample_site %>%
    mutate(active = if_else(is.na(active) | active == "",
                            "No", active)) %>%
    mutate(active_datetime = if_else(!is.na(active_date),
                                     with_tz(as.POSIXct(format(active_date),
                                                        tz = "America/Los_Angeles"), tzone = "UTC"),
                                     active_date)) %>%
    mutate(inactive_datetime = if_else(!is.na(inactive_date),
                                     with_tz(as.POSIXct(format(inactive_date),
                                                        tz = "America/Los_Angeles"), tzone = "UTC"),
                                     inactive_date)) %>%
    mutate(inactive_reason = trimws(inactive_reason)) %>%
    mutate(inactive_reason = if_else(is.na(inactive_reason) | inactive_reason == "",
                                     NA_character_, inactive_reason)) %>%
    mutate(location_description = trimws(location_description)) %>%
    mutate(location_description = if_else(is.na(location_description) | location_description == "",
                                          NA_character_, location_description)) %>%
    mutate(location_comment = trimws(location_comment)) %>%
    mutate(location_comment = if_else(is.na(location_comment) | location_comment == "",
                                      NA_character_, location_comment))
  return(edit_sample_site)
})

dependent_sample_site_surveys = reactive({
  location_id = selected_sample_site_data()$location_id
  sample_site_srv = get_sample_site_surveys(pool, location_id)
  return(sample_site_srv)
})

# Generate values to show check modal
output$sample_site_edit_surveys = renderDT({
  sample_site_edit_srv = dependent_sample_site_surveys() %>%
    select(survey_dt, start_time_dt, end_time_dt, sampler_name)
  sample_site_edit_dt_msg = glue("Multiple surveys, including those below, are linked to this sample site. ",
                                 "Please verify that any edits you make are valid for all linked surveys!")
  # Generate table
  datatable(sample_site_edit_srv,
            colnames = c("Survey date", "Start time", "End time", "Sampler name"),
            rownames = FALSE,
            options = list(dom = 'ltp',
                           pageLength = 5,
                           lengthMenu = c(5, 10),
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: blue; width: auto;',
              htmltools::em(htmltools::strong(sample_site_edit_dt_msg))))
})

# Generate values to show in modal
output$sample_site_modal_update_vals = renderDT({
  sample_site_modal_edit_vals = sample_site_edit() %>%
    mutate(active_date = format(active_date, "%m/%d/%Y")) %>%
    mutate(inactive_date = format(inactive_date, "%m/%d/%Y")) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_date,
           inactive_date, inactive_reason,
           location_description, location_comment)
  # Generate table
  datatable(sample_site_modal_edit_vals,
            colnames = c("Site name", "Site code", "Latitude", "Longitude",
                         "Active?", "Active date", "Inactive date",
                         "Inactive reason", "Site description", "Comment"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Edit modal
observeEvent(input$sample_site_edit, {
  old_sample_site_vals = selected_sample_site_data() %>%
    mutate(location_name = as.character(location_name)) %>%
    mutate(location_code = as.character(location_code)) %>%
    mutate(latitude = round(as.numeric(latitude), 6)) %>%
    mutate(longitude = round(as.numeric(longitude), 6)) %>%
    mutate(active = as.character(active)) %>%
    mutate(active_date = as.character(active_date)) %>%
    mutate(inactive_date = as.character(inactive_date)) %>%
    mutate(inactive_reason = as.character(inactive_reason)) %>%
    mutate(location_description = as.character(location_description)) %>%
    mutate(location_comment = as.character(location_comment)) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_date,
           inactive_date, inactive_reason,
           location_description, location_comment)
  old_sample_site_vals[] = lapply(old_sample_site_vals, set_na)
  new_sample_site_vals = sample_site_edit() %>%
    mutate(location_name = as.character(location_name)) %>%
    mutate(location_code = as.character(location_code)) %>%
    mutate(latitude = round(as.numeric(latitude), 6)) %>%
    mutate(longitude = round(as.numeric(longitude), 6)) %>%
    mutate(active = as.character(active)) %>%
    mutate(active_date = as.character(active_date)) %>%
    mutate(inactive_date = as.character(inactive_date)) %>%
    mutate(inactive_reason = as.character(inactive_reason)) %>%
    mutate(location_description = as.character(location_description)) %>%
    mutate(location_comment = as.character(location_comment)) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_date,
           inactive_date, inactive_reason,
           location_description, location_comment)
  new_sample_site_vals[] = lapply(new_sample_site_vals, set_na)
  showModal(
    tags$div(id = "sample_site_update_modal",
             if ( isTRUE(all_equal(old_sample_site_vals, new_sample_site_vals)) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please change at least one value!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Update sample site info to these new values?",
                 fluidPage (
                   DT::DTOutput("sample_site_modal_update_vals"),
                   br(),
                   br(),
                   DT::DTOutput("sample_site_edit_surveys"),
                   br(),
                   br(),
                   actionButton("save_sample_site_edits", "Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$save_sample_site_edits, {
  tryCatch({
    sample_site_update(pool, sample_site_edit(), selected_sample_site_data())
    shinytoastr::toastr_success("Sample site was edited")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_sample_site_edit_vals = get_sample_sites(pool) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_dt, inactive_dt,
           inactive_reason, location_description,
           location_comment, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(sample_site_dt_proxy, post_sample_site_edit_vals)
}, priority = 9999)

#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$sample_site_modal_delete_vals = renderDT({
  sample_site_modal_del_id = selected_sample_site_data()$location_id
  sample_site_modal_del_vals = get_sample_sites(pool) %>%
    filter(location_id == sample_site_modal_del_id) %>%
    mutate(active_date = format(active_date, "%m/%d/%Y")) %>%
    mutate(inactive_date = format(inactive_date, "%m/%d/%Y")) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_date,
           inactive_date, inactive_reason,
           location_description, location_comment)
  # Generate table
  datatable(sample_site_modal_del_vals,
            colnames = c("Site name", "Site code", "Latitude", "Longitude",
                         "Active?", "Active date", "Inactive date",
                         "Inactive reason", "Site description", "Comment"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Reactive to hold dependencies
sample_site_dependencies = reactive({
  location_id = selected_sample_site_data()$location_id
  sample_site_dep = get_sample_site_dependencies(pool, location_id)
  return(sample_site_dep)
})

# Generate values to show check modal
output$sample_site_delete_surveys = renderDT({
  sample_site_del_srv = dependent_sample_site_surveys() %>%
    select(survey_dt, start_time_dt, end_time_dt, sampler_name)
  sample_site_del_dt_msg = glue("Multiple surveys, including those below, ",
                                "will first need to be reassigned to other sample site!")
  # Generate table
  datatable(sample_site_del_srv,
            colnames = c("Survey date", "Start time", "End time", "Sampler name"),
            rownames = FALSE,
            options = list(dom = 'ltp',
                           pageLength = 5,
                           lengthMenu = c(5, 10),
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: blue; width: auto;',
              htmltools::em(htmltools::strong(sample_site_del_dt_msg))))
})

observeEvent(input$sample_site_delete, {
  location_id = selected_sample_site_data()$location_id
  sample_site_dependencies = sample_site_dependencies()
  table_names = paste0(paste0("'", names(sample_site_dependencies), "'"), collapse = ", ")
  # Customize the delete message depending on if other entries are linked to location
  if ( ncol(sample_site_dependencies) > 0L | nrow(dependent_sample_site_surveys()) > 0L ) {
    sample_site_delete_msg = glue("Other entries in {table_names} have been assigned to this sample site. ",
                                  "Multiple surveys, including those below must first be reassigned to ",
                                  "another sample site before the site can be deleted.")
  } else {
    sample_site_delete_msg = "Are you sure you want to delete this sample site from the database?"
  }
  showModal(
    tags$div(id = "sample_site_delete_modal",
             if ( ncol(sample_site_dependencies) > 0L | nrow(dependent_sample_site_surveys()) > 0L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 sample_site_delete_msg,
                 fluidPage(
                   br(),
                   DT::DTOutput(("sample_site_delete_surveys"))
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = sample_site_delete_msg,
                 fluidPage (
                   DT::DTOutput("sample_site_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_sample_site", "Delete sample site")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_sample_site, {
  tryCatch({
    sample_site_delete(pool, selected_sample_site_data())
    shinytoastr::toastr_success("Sample site was deleted")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  sample_sites_after_delete = get_sample_sites(pool) %>%
    select(location_name, location_code, latitude,
           longitude, active, active_dt, inactive_dt,
           inactive_reason, location_description,
           location_comment, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(sample_site_dt_proxy, sample_sites_after_delete)
}, priority = 9999)



