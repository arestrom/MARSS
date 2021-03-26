#========================================================
# Generate lut select ui's
#========================================================

output$catch_area_select = renderUI({
  req(valid_connection == TRUE)
  catch_area_list = get_catch_area(pool)$catch_area
  catch_area_list = c("", catch_area_list)
  selectizeInput("catch_area_select", label = "CRC area",
                 choices = catch_area_list, selected = NULL,
                 width = "75px")
})

output$fishing_method_select = renderUI({
  req(valid_connection == TRUE)
  fish_method_list = get_fishing_method(pool)$fishing_method
  fish_method_list = c("", fish_method_list)
  selectizeInput("fishing_method_select", label = "Fish Meth",
                 choices = fish_method_list, selected = NULL,
                 width = "125px")
})

output$cooperative_angler_select = renderUI({
  req(valid_connection == TRUE)
  cooperative_angler_list = c("Yes", "No")
  selectizeInput("cooperative_angler_select", label = "Cooperative?",
                 choices = cooperative_angler_list, selected = "Yes",
                 width = "75px")
})

output$target_species_select = renderUI({
  req(valid_connection == TRUE)
  target_species_list = get_target_species(pool)$target_species
  target_species_list = c("", target_species_list)
  selectizeInput("target_species_select", label = "Targ Sps",
                 choices = target_species_list, selected = NULL,
                 width = "150px")
})

#========================================================
# Primary datatable for interview_events
#========================================================

# Primary DT datatable for survey_intent
output$interview_events = renderDT({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  interview_event_title = glue("Interview data for {selected_survey_data()$survey_site} on ",
                               "{selected_survey_data()$survey_date}")
  interview_event_data = get_interview_event(pool, selected_survey_data()$survey_id) %>%
    select(interview_number, catch_area, fishing_method, angler_count,
           cooperative_angler, created_dt, created_by, modified_dt,
           modified_by)

  # Generate table
  datatable(interview_event_data,
            colnames = c("Intvw", "CRC", "Fish Meth", "Anglrs", "Cooperative?",
                         "Create DT", "Create By", "Mod DT", "Mod By"),
            selection = list(mode = 'single'),
            options = list(dom = 'ltp',
                           pageLength = 5,
                           lengthMenu = c(5, 10, 25, 50, 200, 500),
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(interview_event_title))))
})

# Create surveys DT proxy object
interview_event_dt_proxy = dataTableProxy(outputId = "interview_events")

#========================================================
# Collect event values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_interview_event_data = reactive({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  interview_event_data = get_interview_event(pool, selected_survey_data()$survey_id)
  interview_event_row = input$interview_events_rows_selected
  selected_interview_event = tibble(survey_event_id = interview_event_data$survey_event_id[interview_event_row],
                                    interview_number = interview_event_data$interview_number[interview_event_row],
                                    catch_area = interview_event_data$catch_area[interview_event_row],
                                    fishing_method = interview_event_data$fishing_method[interview_event_row],
                                    cooperative_angler = interview_event_data$cooperative_angler[interview_event_row],
                                    angler_count = interview_event_data$angler_count[interview_event_row],
                                    created_dt = interview_event_data$created_dt[interview_event_row],
                                    created_by = interview_event_data$created_by[interview_event_row],
                                    modified_dt = interview_event_data$modified_dt[interview_event_row],
                                    modified_by = interview_event_data$modified_by[interview_event_row])
  return(selected_interview_event)
})

#========================================================
# Update event select inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$interview_events_rows_selected, {
  siedat = selected_interview_event_data()
  updateNumericInput(session, "interview_number_input", value = siedat$interview_number)
  updateSelectizeInput(session, "catch_area_select", selected = siedat$catch_area)
  updateSelectizeInput(session, "fishing_method_select", selected = siedat$fishing_method)
  updateSelectizeInput(session, "cooperative_angler_select", selected = siedat$cooperative_angler)
  updateNumericInput(session, "angler_count_input", value = siedat$angler_count)
})


#========================================================
# Primary datatable for dockside_events
#========================================================

# Primary DT datatable for dockside encounters
output$dockside_events = renderDT({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  dockside_event_title = glue("Trip and fish times")
  dockside_event_data = get_dockside_event(pool, selected_interview_event_data()$survey_event_id) %>%
    select(trip_start, fish_start, fish_end, trip_end, no_license,
           created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(dockside_event_data,
            colnames = c("Trip start", "Fish start", "Fish end", "Trip end",
                         "No lic.", "Create DT", "Create By", "Mod DT", "Mod By"),
            selection = list(mode = 'single'),
            options = list(dom = 'ltp',
                           pageLength = 5,
                           lengthMenu = c(5, 10, 25, 50),
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(dockside_event_title))))
})

# Create surveys DT proxy object
dockside_event_dt_proxy = dataTableProxy(outputId = "dockside_events")

#========================================================
# Collect event values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_dockside_event_data = reactive({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  req(input$dockside_events_rows_selected)
  dockside_event_data = get_dockside_event(pool, selected_interview_event_data()$survey_event_id)
  dockside_event_row = input$dockside_events_rows_selected
  selected_dockside_event = tibble(dockside_encounter_id = dockside_event_data$dockside_encounter_id[dockside_event_row],
                                   trip_start = dockside_event_data$trip_start_datetime[dockside_event_row],
                                   fish_start = dockside_event_data$fish_start_datetime[dockside_event_row],
                                   fish_end = dockside_event_data$fish_end_datetime[dockside_event_row],
                                   trip_end = dockside_event_data$trip_end_datetime[dockside_event_row],
                                   no_license = dockside_event_data$no_license[dockside_event_row],
                                   created_dt = dockside_event_data$created_dt[dockside_event_row],
                                   created_by = dockside_event_data$created_by[dockside_event_row],
                                   modified_dt = dockside_event_data$modified_dt[dockside_event_row],
                                   modified_by = dockside_event_data$modified_by[dockside_event_row])
  return(selected_dockside_event)
})

#========================================================
# Update event select inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$dockside_events_rows_selected, {
  deedat = selected_dockside_event_data()
  # Account for cases where time values are missing
  if (is.na(deedat$trip_start) ) {
    trip_start = NULL
  } else {
    trip_start = deedat$trip_start
  }
  if (is.na(deedat$fish_start) ) {
    fish_start = NULL
  } else {
    fish_start = deedat$fish_start
  }
  if (is.na(deedat$fish_end) ) {
    fish_end = NULL
  } else {
    fish_end = deedat$fish_end
  }
  if (is.na(deedat$trip_end) ) {
    trip_end = NULL
  } else {
    trip_end = deedat$trip_end
  }
  # Update widgets
  updateAirDateInput(session, "trip_start_input", value = trip_start, clear = TRUE)
  updateAirDateInput(session, "fish_start_input", value = fish_start, clear = TRUE)
  updateAirDateInput(session, "fish_end_input", value = fish_end, clear = TRUE)
  updateAirDateInput(session, "trip_end_input", value = trip_end, clear = TRUE)
  updateNumericInput(session, "no_license_input", value = deedat$no_license)
})

#========================================================
# Primary datatable for target_events
#========================================================

# Primary DT datatable for target species
output$target_events = renderDT({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  req(input$dockside_events_rows_selected)
  target_event_title = glue("Target species")
  target_event_data = get_target_event(pool, selected_interview_event_data()$survey_event_id) %>%
    select(target_species, created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(target_event_data,
            colnames = c("Targ Sps", "Create DT", "Create By", "Mod DT", "Mod By"),
            selection = list(mode = 'single'),
            options = list(dom = 'ltp',
                           pageLength = 5,
                           lengthMenu = c(5, 10, 25, 50),
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(target_event_title))))
})

# Create surveys DT proxy object
target_event_dt_proxy = dataTableProxy(outputId = "target_events")

#========================================================
# Collect event values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_target_event_data = reactive({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  req(input$dockside_events_rows_selected)
  req(input$target_events_rows_selected)
  target_event_data = get_target_event(pool, selected_interview_event_data()$survey_event_id)
  target_event_row = input$target_events_rows_selected
  selected_target_event = tibble(target_species_id = target_event_data$target_species_id[target_event_row],
                                 target_species = target_event_data$target_species[target_event_row],
                                 created_dt = target_event_data$created_dt[target_event_row],
                                 created_by = target_event_data$created_by[target_event_row],
                                 modified_dt = target_event_data$modified_dt[target_event_row],
                                 modified_by = target_event_data$modified_by[target_event_row])
  return(selected_target_event)
})

#========================================================
# Update event select inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$target_events_rows_selected, {
  stedat = selected_target_event_data()
  updateSelectizeInput(session, "target_species_select", selected = stedat$target_species)
})

#========================================================
# Interview insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
interview_event_create = reactive({
  req(input$surveys_rows_selected)
  # Survey_id
  survey_id_input = selected_survey_data()$survey_id
  # Catch area
  catch_area_input = input$catch_area_select
  if (catch_area_input == "" ) {
    catch_area_id = NA_character_
  } else {
    catch_area_vals = get_catch_area(pool)
    catch_area_id = catch_area_vals %>%
      filter(catch_area == catch_area_input) %>%
      pull(location_id)
  }
  # Fishing method
  fishing_method_input = input$fishing_method_select
  if (fishing_method_input == "" ) {
    fishing_method_id = NA_character_
  } else {
    fishing_method_vals = get_fishing_method(pool)
    fishing_method_id = fishing_method_vals %>%
      filter(fishing_method == fishing_method_input) %>%
      pull(fishing_method_id)
  }
  new_interview_event = tibble(survey_id = survey_id_input,
                               interview_number = input$interview_number_input,
                               catch_area = catch_area_input,
                               catch_area_id = catch_area_id,
                               fishing_method = fishing_method_input,
                               fishing_method_id = fishing_method_id,
                               cooperative_angler = input$cooperative_angler_select,
                               angler_count = input$angler_count_input,
                               created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                               created_by = Sys.getenv("USERNAME"))
  new_interview_event = new_interview_event %>%
    mutate(interview_number = as.integer(interview_number)) %>%
    mutate(cooperative_angler = if_else(is.na(cooperative_angler) |
                                          cooperative_angler == "",
                                        "No", cooperative_angler)) %>%
    mutate(angler_count = as.integer(angler_count))
  return(new_interview_event)
})

# Generate values to show in modal
output$interview_event_modal_insert_vals = renderDT({
  interview_event_modal_in_vals = interview_event_create() %>%
    select(interview_number, catch_area, fishing_method,
           angler_count, cooperative_angler)
  # Generate table
  datatable(interview_event_modal_in_vals,
            colnames = c("Intvw", "CRC", "Fish Meth", "Anglrs", "Cooperative?"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new interview events
observeEvent(input$interview_add, {
  new_interview_event_vals = interview_event_create()
  showModal(
    # Verify all fields have data...none can be blank
    tags$div(id = "interview_event_insert_modal",
             if ( is.na(new_interview_event_vals$interview_number) |
                  is.na(new_interview_event_vals$catch_area_id) |
                  is.na(new_interview_event_vals$fishing_method_id) |
                  is.na(new_interview_event_vals$cooperative_angler) |
                  is.na(new_interview_event_vals$angler_count) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required in all fields highlighted in green"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new interview data to the database?"),
                 fluidPage (
                   DT::DTOutput("interview_event_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_interview_event", "Insert interview data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
interview_event_insert_vals = reactive({
  new_interview_values = interview_event_create() %>%
    select(survey_id, interview_number,
           uncooperative_angler_indicator = cooperative_angler,
           angler_count, catch_area_id, fishing_method_id,
           created_by)
  return(new_interview_values)
})

# Update DB and reload DT
observeEvent(input$insert_interview_event, {
  tryCatch({
    interview_event_insert(pool, interview_event_insert_vals())
    shinytoastr::toastr_success("New interview data was added")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_interview_insert_vals = get_interview_event(pool, selected_survey_data()$survey_id) %>%
    select(interview_number, catch_area, fishing_method, angler_count,
           cooperative_angler, created_dt, created_by, modified_dt,
           modified_by)
  replaceData(interview_event_dt_proxy, post_interview_insert_vals)
})

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
interview_event_edit = reactive({
  req(input$interview_events_rows_selected)
  # Survey_id
  survey_event_id_input = selected_interview_event_data()$survey_event_id
  # Catch area
  catch_area_input = input$catch_area_select
  if (catch_area_input == "" ) {
    catch_area_id = NA_character_
  } else {
    catch_area_vals = get_catch_area(pool)
    catch_area_id = catch_area_vals %>%
      filter(catch_area == catch_area_input) %>%
      pull(location_id)
  }
  # Fishing method
  fishing_method_input = input$fishing_method_select
  if (fishing_method_input == "" ) {
    fishing_method_id = NA_character_
  } else {
    fishing_method_vals = get_fishing_method(pool)
    fishing_method_id = fishing_method_vals %>%
      filter(fishing_method == fishing_method_input) %>%
      pull(fishing_method_id)
  }
  edit_interview_event = tibble(survey_event_id = survey_event_id_input,
                                interview_number = input$interview_number_input,
                                catch_area = catch_area_input,
                                catch_area_id = catch_area_id,
                                fishing_method = fishing_method_input,
                                fishing_method_id = fishing_method_id,
                                cooperative_angler = input$cooperative_angler_select,
                                angler_count = input$angler_count_input,
                                modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
                                modified_by = Sys.getenv("USERNAME"))
  edit_interview_event = edit_interview_event %>%
    mutate(interview_number = as.integer(interview_number)) %>%
    mutate(cooperative_angler = if_else(is.na(cooperative_angler) |
                                          cooperative_angler == "",
                                        "No", cooperative_angler)) %>%
    mutate(angler_count = as.integer(angler_count))
  return(edit_interview_event)
})

# Generate values to show in modal
output$interview_event_modal_update_vals = renderDT({
  interview_event_modal_up_vals = interview_event_edit() %>%
    select(interview_number, catch_area, fishing_method,
           angler_count, cooperative_angler)
  # Generate table
  datatable(interview_event_modal_up_vals,
            colnames = c("Intvw", "CRC", "Fish Meth", "Anglrs", "Cooperative?"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$interview_edit, {
  new_interview_event_vals = interview_event_edit() %>%
    mutate(interview_number = as.integer(interview_number)) %>%
    mutate(catch_area = as.character(catch_area)) %>%
    mutate(fishing_method = as.character(fishing_method)) %>%
    mutate(angler_count = as.integer(angler_count)) %>%
    mutate(cooperative_angler = as.character(cooperative_angler)) %>%
    select(interview_number, catch_area, fishing_method,
           angler_count, cooperative_angler)
  old_interview_event_vals = selected_interview_event_data() %>%
    mutate(interview_number = as.integer(interview_number)) %>%
    mutate(catch_area = as.character(catch_area)) %>%
    mutate(fishing_method = as.character(fishing_method)) %>%
    mutate(angler_count = as.integer(angler_count)) %>%
    mutate(cooperative_angler = as.character(cooperative_angler)) %>%
    select(interview_number, catch_area, fishing_method,
           angler_count, cooperative_angler)
  showModal(
    tags$div(id = "interview_event_update_modal",
             if ( isTRUE(all_equal(old_interview_event_vals, new_interview_event_vals)) ) {
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
                 title = "Update interview data to these new values?",
                 fluidPage (
                   DT::DTOutput("interview_event_modal_update_vals"),
                   br(),
                   br(),
                   actionButton("save_interview_event_edits","Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$save_interview_event_edits, {
  tryCatch({
    interview_event_update(pool, interview_event_edit() )
    shinytoastr::toastr_success("Interview data was edited")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_interview_event_edit_vals = get_interview_event(pool, selected_survey_data()$survey_id) %>%
    select(interview_number, catch_area, fishing_method, angler_count,
           cooperative_angler, created_dt, created_by, modified_dt,
           modified_by)
  replaceData(interview_event_dt_proxy, post_interview_event_edit_vals)
})

#========================================================
# Interview delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$interview_event_modal_delete_vals = renderDT({
  interview_event_modal_del_id = selected_interview_event_data()$survey_event_id
  interview_event_modal_del_vals = get_interview_event(pool, selected_survey_data()$survey_id) %>%
    filter(survey_event_id == interview_event_modal_del_id) %>%
    select(interview_number, catch_area, fishing_method,
           angler_count, cooperative_angler)
  # Generate table
  datatable(interview_event_modal_del_vals,
            colnames = c("Intvw", "CRC", "Fish Meth", "Anglrs", "Cooperative?"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$interview_delete, {
  survey_event_id = selected_interview_event_data()$survey_event_id
  interview_event_dependencies = get_interview_event_dependencies(pool, survey_event_id)
  table_names = paste0(names(interview_event_dependencies), collapse = ", ")
  #table_names = gsub("survey_event", "species_data", table_names)
  showModal(
    tags$div(id = "interview_event_delete_modal",
             if ( ncol(interview_event_dependencies) > 0L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 glue("Please delete dependent data from the following tables first: {table_names}"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Are you sure you want to delete this interview data from the database?",
                 fluidPage (
                   DT::DTOutput("interview_event_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_interview_event", "Delete interview")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_interview_event, {
  tryCatch({
    interview_event_delete(pool, selected_interview_event_data() )
    shinytoastr::toastr_success("Interview data was deleted")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  interview_events_after_delete = get_interview_event(pool, selected_survey_data()$survey_id) %>%
    select(interview_number, catch_area, fishing_method, angler_count,
           cooperative_angler, created_dt, created_by, modified_dt,
           modified_by)
  replaceData(interview_event_dt_proxy, interview_events_after_delete)
})

#====================================================================
# Dockside event insert operations: reactives, observers and modals
#====================================================================

# Create reactive to collect input values for insert actions
dockside_event_create = reactive({
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  # Survey_event_id
  survey_event_id_input = selected_interview_event_data()$survey_event_id
  # Time values
  trip_start = format(input$trip_start_input)
  if (nchar(trip_start) < 16) { trip_start = NA_character_ }
  trip_start = as.POSIXct(trip_start)
  fish_start = format(input$fish_start_input)
  if (nchar(fish_start) < 16) { fish_start = NA_character_ }
  fish_start = as.POSIXct(fish_start)
  fish_end = format(input$fish_end_input)
  if (nchar(fish_end) < 16) { fish_end = NA_character_ }
  fish_end = as.POSIXct(fish_end)
  trip_end = format(input$trip_end_input)
  if (nchar(trip_end) < 16) { trip_end = NA_character_ }
  trip_end = as.POSIXct(trip_end)
  new_dockside_event = tibble(survey_event_id = survey_event_id_input,
                              trip_start = trip_start,
                              fish_start = fish_start,
                              fish_end = fish_end,
                              trip_end = trip_end,
                              no_license = input$no_license_input,
                              created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                              created_by = Sys.getenv("USERNAME"))
  new_dockside_event = new_dockside_event %>%
    mutate(no_license = as.integer(no_license))
  return(new_dockside_event)
})

# Generate values to show in modal
output$dockside_event_modal_insert_vals = renderDT({
  dockside_event_modal_in_vals = dockside_event_create() %>%
    mutate(trip_start = format(trip_start, "%m/%d/%Y %H:%M")) %>%
    mutate(fish_start = format(fish_start, "%m/%d/%Y %H:%M")) %>%
    mutate(fish_end = format(fish_end, "%m/%d/%Y %H:%M")) %>%
    mutate(trip_end = format(trip_end, "%m/%d/%Y %H:%M")) %>%
    mutate(trip_start = if_else(substr(trip_start, 11, 15) == "00:00", "", trip_start)) %>%
    mutate(trip_end = if_else(substr(trip_end, 11, 15) == "00:00", "", trip_end)) %>%
    select(trip_start, fish_start, fish_end, trip_end, no_license)
  # Generate table
  datatable(dockside_event_modal_in_vals,
            colnames = c("Trip start", "Fish start", "Fish end", "Trip end", "No lic."),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new dockside events
observeEvent(input$dockside_add, {
  new_dockside_event_vals = dockside_event_create()
  showModal(
    # Verify all fields have data...none can be blank
    tags$div(id = "dockside_event_insert_modal",
             if ( is.na(new_dockside_event_vals$fish_start) |
                  is.na(new_dockside_event_vals$fish_end) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required in all fields highlighted in green"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new trip and fish time data to the database?"),
                 fluidPage (
                   DT::DTOutput("dockside_event_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_dockside_event", "Insert trip and fish time data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
dockside_event_insert_vals = reactive({
  new_dockside_event_values = dockside_event_create() %>%
    mutate(trip_start = with_tz(trip_start, tzone = "UTC")) %>%
    mutate(fish_start = with_tz(fish_start, tzone = "UTC")) %>%
    mutate(fish_end = with_tz(fish_end, tzone = "UTC")) %>%
    mutate(trip_end = with_tz(trip_end, tzone = "UTC")) %>%
    select(survey_event_id, trip_start, fish_start,
           fish_end, trip_end, no_license, created_by)
  return(new_dockside_event_values)
})

# Update DB and reload DT
observeEvent(input$insert_dockside_event, {
  tryCatch({
    dockside_event_insert(pool, dockside_event_insert_vals())
    shinytoastr::toastr_success("New trip and fish time data was added")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_dockside_event_insert_vals = get_dockside_event(pool, selected_interview_event_data()$survey_event_id)  %>%
    select(trip_start, fish_start, fish_end, trip_end, no_license,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(dockside_event_dt_proxy, post_dockside_event_insert_vals)
})

#========================================================
# Dockside event edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
dockside_event_edit = reactive({
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  req(input$dockside_events_rows_selected)
  # Survey_event_id
  dockside_encounter_id_input = selected_dockside_event_data()$dockside_encounter_id
  # Time values
  trip_start = format(input$trip_start_input)
  if (nchar(trip_start) < 16) { trip_start = NA_character_ }
  trip_start = as.POSIXct(trip_start)
  fish_start = format(input$fish_start_input)
  if (nchar(fish_start) < 16) { fish_start = NA_character_ }
  fish_start = as.POSIXct(fish_start)
  fish_end = format(input$fish_end_input)
  if (nchar(fish_end) < 16) { fish_end = NA_character_ }
  fish_end = as.POSIXct(fish_end)
  trip_end = format(input$trip_end_input)
  if (nchar(trip_end) < 16) { trip_end = NA_character_ }
  trip_end = as.POSIXct(trip_end)
  edit_dockside_event = tibble(dockside_encounter_id = dockside_encounter_id_input,
                               trip_start = trip_start,
                               fish_start = fish_start,
                               fish_end = fish_end,
                               trip_end = trip_end,
                               no_license = input$no_license_input,
                               created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                               created_by = Sys.getenv("USERNAME"))
  edit_dockside_event = edit_dockside_event %>%
    mutate(no_license = as.integer(no_license))
  return(edit_dockside_event)
})

# Generate values to show in modal
output$dockside_event_modal_edit_vals = renderDT({
  dockside_event_modal_up_vals = dockside_event_edit() %>%
    mutate(trip_start = format(trip_start, "%m/%d/%Y %H:%M")) %>%
    mutate(fish_start = format(fish_start, "%m/%d/%Y %H:%M")) %>%
    mutate(fish_end = format(fish_end, "%m/%d/%Y %H:%M")) %>%
    mutate(trip_end = format(trip_end, "%m/%d/%Y %H:%M")) %>%
    mutate(trip_start = if_else(substr(trip_start, 11, 15) == "00:00", "", trip_start)) %>%
    mutate(trip_end = if_else(substr(trip_end, 11, 15) == "00:00", "", trip_end)) %>%
    select(trip_start, fish_start, fish_end, trip_end, no_license)
  # Generate table
  datatable(dockside_event_modal_up_vals,
            colnames = c("Trip start", "Fish start", "Fish end", "Trip end", "No lic."),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new dockside events
observeEvent(input$dockside_edit, {
  edit_dockside_event_vals = dockside_event_edit()
  showModal(
    # Verify all fields have data...none can be blank
    tags$div(id = "dockside_event_edit_modal",
             if ( is.na(edit_dockside_event_vals$fish_start) |
                  is.na(edit_dockside_event_vals$fish_end) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required in all fields highlighted in green"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Update trip and fish time data in the database?"),
                 fluidPage (
                   DT::DTOutput("dockside_event_modal_edit_vals"),
                   br(),
                   br(),
                   actionButton("edit_dockside_event", "Update trip and fish time data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$edit_dockside_event, {
  tryCatch({
    dockside_event_update(pool, dockside_event_edit())
    shinytoastr::toastr_success("Trip and fish time data was updated")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_dockside_event_edit_vals = get_dockside_event(pool, selected_interview_event_data()$survey_event_id)  %>%
    select(trip_start, fish_start, fish_end, trip_end, no_license,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(dockside_event_dt_proxy, post_dockside_event_edit_vals)
})

#========================================================
# Dockside delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$dockside_event_modal_delete_vals = renderDT({
  dockside_event_modal_del_id = selected_dockside_event_data()$dockside_encounter_id
  dockside_event_modal_del_vals = get_dockside_event(pool, selected_interview_event_data()$survey_event_id) %>%
    filter(dockside_encounter_id == dockside_event_modal_del_id) %>%
    select(trip_start, fish_start, fish_end, trip_end, no_license)
  # Generate table
  datatable(dockside_event_modal_del_vals,
            colnames = c("Trip start", "Fish start", "Fish end", "Trip end", "No lic."),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$dockside_delete, {
  dockside_encounter_id = selected_dockside_event_data()$dockside_encounter_id
  showModal(
    tags$div(id = "dockside_event_delete_modal",
             modalDialog (
               size = 'l',
               title = "Are you sure you want to delete this trip and fish time data from the database?",
               fluidPage (
                 DT::DTOutput("dockside_event_modal_delete_vals"),
                 br(),
                 br(),
                 actionButton("delete_dockside_event", "Delete trip and fish times")
               ),
               easyClose = TRUE,
               footer = NULL
             )
    ))
})

# Update DB and reload DT
observeEvent(input$delete_dockside_event, {
  tryCatch({
    dockside_event_delete(pool, selected_dockside_event_data() )
    shinytoastr::toastr_success("Trip and fish time data was deleted")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  dockside_events_after_delete = get_dockside_event(pool, selected_interview_event_data()$survey_event_id) %>%
    select(trip_start, fish_start, fish_end, trip_end, no_license,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(dockside_event_dt_proxy, dockside_events_after_delete)
})

#========================================================
# Target species insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
target_event_create = reactive({
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  # Survey_event_id
  survey_event_id_input = selected_interview_event_data()$survey_event_id
  # Target species
  target_species_input = input$target_species_select
  if (target_species_input == "" ) {
    target_species_type_id = NA_character_
  } else {
    target_species_vals = get_target_species(pool)
    target_species_type_id = target_species_vals %>%
      filter(target_species == target_species_input) %>%
      pull(target_species_type_id)
  }
  new_target_event = tibble(survey_event_id = survey_event_id_input,
                            target_species = target_species_input,
                            target_species_type_id = target_species_type_id,
                            created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                            created_by = Sys.getenv("USERNAME"))
  return(new_target_event)
})

# Generate values to show in modal
output$target_event_modal_insert_vals = renderDT({
  target_event_modal_in_vals = target_event_create() %>%
    select(target_species)
  # Generate table
  datatable(target_event_modal_in_vals,
            colnames = c("Target Species"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new intents. Need a dup flag, multiple rows possible
observeEvent(input$target_add, {
  new_target_event_vals = target_event_create()
  showModal(
    # Verify all fields have data...none can be blank
    tags$div(id = "target_event_insert_modal",
             if ( is.na(new_target_event_vals$target_species) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required in all fields highlighted in green"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'm',
                 title = glue("Insert new target species data to the database?"),
                 fluidPage (
                   DT::DTOutput("target_event_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_target_event", "Insert target species data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
target_event_insert_vals = reactive({
  new_target_event_values = target_event_create() %>%
    select(survey_event_id, target_species_type_id, created_by)
  return(new_target_event_values)
})

# Update DB and reload DT
observeEvent(input$insert_target_event, {
  tryCatch({
    target_event_insert(pool, target_event_insert_vals())
    shinytoastr::toastr_success("New target species data was added")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_target_event_insert_vals = get_target_event(pool, selected_interview_event_data()$survey_event_id) %>%
    select(target_species, created_dt, created_by, modified_dt, modified_by)
  replaceData(target_event_dt_proxy, post_target_event_insert_vals)
})

#========================================================
# Target species edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
target_event_edit = reactive({
  req(input$surveys_rows_selected)
  req(input$target_events_rows_selected)
  # Target_species_id
  target_species_id_input = selected_target_event_data()$target_species_id
  # Target species
  target_species_input = input$target_species_select
  if (target_species_input == "" ) {
    target_species_type_id = NA_character_
  } else {
    target_species_vals = get_target_species(pool)
    target_species_type_id = target_species_vals %>%
      filter(target_species == target_species_input) %>%
      pull(target_species_type_id)
  }
  edit_target_event = tibble(target_species_id = target_species_id_input,
                             target_species = target_species_input,
                             target_species_type_id = target_species_type_id,
                             created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                             created_by = Sys.getenv("USERNAME"))
  return(edit_target_event)
})

# Generate values to show in modal
output$target_event_modal_edit_vals = renderDT({
  target_event_modal_up_vals = target_event_edit() %>%
    select(target_species)
  # Generate table
  datatable(target_event_modal_up_vals,
            colnames = c("Target Species"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for edits
observeEvent(input$target_edit, {
  edit_target_event_vals = target_event_edit()
  showModal(
    # Verify all fields have data...none can be blank
    tags$div(id = "target_event_insert_modal",
             if ( is.na(edit_target_event_vals$target_species) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required in all fields highlighted in green"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'm',
                 title = glue("Update target species data to the database?"),
                 fluidPage (
                   DT::DTOutput("target_event_modal_edit_vals"),
                   br(),
                   br(),
                   actionButton("update_target_event", "Update target species data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$update_target_event, {
  tryCatch({
    target_event_update( pool, target_event_edit() )
    shinytoastr::toastr_success("Target species data was updated")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_target_event_edit_vals = get_target_event(pool, selected_interview_event_data()$survey_event_id) %>%
    select(target_species, created_dt, created_by, modified_dt, modified_by)
  replaceData(target_event_dt_proxy, post_target_event_edit_vals)
})


#========================================================
# Target event delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$target_event_modal_delete_vals = renderDT({
  target_event_modal_del_id = selected_target_event_data()$target_species_id
  target_event_modal_del_vals = get_target_event(pool, selected_interview_event_data()$survey_event_id) %>%
    filter(target_species_id == target_event_modal_del_id) %>%
    select(target_species)
  # Generate table
  datatable(target_event_modal_del_vals,
            colnames = c("Target Species"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$target_delete, {
  req(input$target_events_rows_selected)
  showModal(
    tags$div(id = "target_event_delete_modal",
             modalDialog (
               size = 'l',
               title = "Are you sure you want to delete this target species data from the database?",
               fluidPage (
                 DT::DTOutput("target_event_modal_delete_vals"),
                 br(),
                 br(),
                 actionButton("delete_target_event", "Delete target species data")
               ),
               easyClose = TRUE,
               footer = NULL
             )
    ))
})

# Update DB and reload DT
observeEvent(input$delete_target_event, {
  tryCatch({
    target_event_delete( pool, selected_target_event_data() )
    shinytoastr::toastr_success("Target species data was deleted")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  target_events_after_delete = get_target_event(pool, selected_interview_event_data()$survey_event_id) %>%
    select(target_species, created_dt, created_by, modified_dt, modified_by)
  replaceData(target_event_dt_proxy, target_events_after_delete)
})

