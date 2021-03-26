#========================================================
# Generate lut select ui's
#========================================================

output$event_species_select = renderUI({
  req(valid_connection == TRUE)
  event_species_list = get_event_species(pool)$event_species
  event_species_list = c("", event_species_list)
  selectizeInput("event_species_select", label = "Species",
                 choices = event_species_list, selected = NULL,
                 width = "150px")
})

output$catch_result_select = renderUI({
  req(valid_connection == TRUE)
  catch_result_list = get_catch_result(pool)$catch_result
  catch_result_list = c("", catch_result_list)
  selectizeInput("catch_result_select", label = "Kept?",
                 choices = catch_result_list, selected = NULL,
                 width = "150px")
})

output$ad_mark_select = renderUI({
  req(valid_connection == TRUE)
  ad_mark_list = get_ad_mark(pool)$ad_mark
  ad_mark_list = c("", ad_mark_list)
  selectizeInput("ad_mark_select", label = "Mark",
                 choices = ad_mark_list, selected = NULL,
                 width = "75px")
})

output$depth_range_select = renderUI({
  req(valid_connection == TRUE)
  depth_range_list = get_depth_range(pool)$depth_range
  depth_range_list = c("", depth_range_list)
  selectizeInput("depth_range_select", label = "Depth range",
                 choices = depth_range_list, selected = NULL,
                 width = "175px")
})

output$gear_type_select = renderUI({
  req(valid_connection == TRUE)
  gear_type_list = get_gear_type(pool)$gear_type
  gear_type_list = c("", gear_type_list)
  selectizeInput("gear_type_select", label = "Gear",
                 choices = gear_type_list, selected = NULL,
                 width = "225px")
})

#========================================================
# Primary datatable for survey_events
#========================================================

# Primary DT datatable for survey_intent
output$fish_encounters = renderDT({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  fish_encounter_title = glue("Fish encounters for {selected_survey_data()$survey_site} on ",
                              "{selected_survey_data()$survey_date}")
  fish_encounter_data = get_fish_encounter(pool, selected_interview_event_data()$survey_event_id) %>%
    select(species, fish_count, catch_result, ad_mark, depth_range, gear_type,
           created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(fish_encounter_data,
            colnames = c("Species", "Fish count", "Kept?", "Mark", "Depth range", "Gear",
                         "Create DT", "Create By", "Mod DT", "Mod By"),
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
              htmltools::em(htmltools::strong(fish_encounter_title))))
})

# Create surveys DT proxy object
fish_encounter_dt_proxy = dataTableProxy(outputId = "fish_encounters")

#========================================================
# Collect event values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_fish_encounter_data = reactive({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  fish_encounter_data = get_fish_encounter(pool, selected_interview_event_data()$survey_event_id)
  fish_encounter_row = input$fish_encounters_rows_selected
  selected_fish_encounter = tibble(fish_encounter_id = fish_encounter_data$fish_encounter_id[fish_encounter_row],
                                   species = fish_encounter_data$species[fish_encounter_row],
                                   fish_count = fish_encounter_data$fish_count[fish_encounter_row],
                                   catch_result = fish_encounter_data$catch_result[fish_encounter_row],
                                   ad_mark = fish_encounter_data$ad_mark[fish_encounter_row],
                                   depth_range = fish_encounter_data$depth_range[fish_encounter_row],
                                   gear_type = fish_encounter_data$gear_type[fish_encounter_row],
                                   created_dt = fish_encounter_data$created_dt[fish_encounter_row],
                                   created_by = fish_encounter_data$created_by[fish_encounter_row],
                                   modified_dt = fish_encounter_data$modified_dt[fish_encounter_row],
                                   modified_by = fish_encounter_data$modified_by[fish_encounter_row])
  return(selected_fish_encounter)
})

#========================================================
# Update event select inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$fish_encounters_rows_selected, {
  sfedat = selected_fish_encounter_data()
  updateSelectizeInput(session, "event_species_select", selected = sfedat$species)
  updateNumericInput(session, "fish_count_input", value = sfedat$fish_count)
  updateSelectizeInput(session, "catch_result_select", selected = sfedat$catch_result)
  updateSelectizeInput(session, "ad_mark_select", selected = sfedat$ad_mark)
  updateSelectizeInput(session, "depth_range_select", selected = sfedat$depth_range)
  updateSelectizeInput(session, "gear_type_select", selected = sfedat$gear_type)
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
fish_event_create = reactive({
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  # Survey_event_id
  survey_event_id_input = selected_interview_event_data()$survey_event_id
  # Species
  event_species_input = input$event_species_select
  if (event_species_input == "" ) {
    event_species_id = NA_character_
  } else {
    event_species_vals = get_event_species(pool)
    event_species_id = event_species_vals %>%
      filter(event_species == event_species_input) %>%
      pull(event_species_id)
  }
  # Catch result
  catch_result_input = input$catch_result_select
  if (catch_result_input == "" ) {
    catch_result_type_id = NA_character_
  } else {
    catch_result_vals = get_catch_result(pool)
    catch_result_type_id = catch_result_vals %>%
      filter(catch_result == catch_result_input) %>%
      pull(catch_result_type_id)
  }
  # AD mark
  ad_mark_input = input$ad_mark_select
  if ( ad_mark_input == "" ) {
    adipose_clip_status_id = NA_character_
  } else {
    ad_mark_vals = get_ad_mark(pool)
    adipose_clip_status_id = ad_mark_vals %>%
      filter(ad_mark == ad_mark_input) %>%
      pull(adipose_clip_status_id)
  }
  # Depth range
  depth_range_input = input$depth_range_select
  if ( depth_range_input == "" ) {
    encounter_depth_range_id = NA_character_
  } else {
    depth_range_vals = get_depth_range(pool)
    encounter_depth_range_id = depth_range_vals %>%
      filter(depth_range == depth_range_input) %>%
      pull(encounter_depth_range_id)
  }
  # Gear type
  gear_type_input = input$gear_type_select
  if ( gear_type_input == "" ) {
    encounter_gear_type_id = NA_character_
  } else {
    gear_type_vals = get_gear_type(pool)
    encounter_gear_type_id = gear_type_vals %>%
      filter(gear_type == gear_type_input) %>%
      pull(encounter_gear_type_id)
  }
  new_fish_event = tibble(survey_event_id = survey_event_id_input,
                          species = event_species_input,
                          species_id = event_species_id,
                          fish_count = input$fish_count_input,
                          catch_result = catch_result_input,
                          catch_result_type_id = catch_result_type_id,
                          ad_mark = ad_mark_input,
                          adipose_clip_status_id = adipose_clip_status_id,
                          depth_range = depth_range_input,
                          encounter_depth_range_id = encounter_depth_range_id,
                          gear_type = gear_type_input,
                          encounter_gear_type_id = encounter_gear_type_id,
                          created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                          created_by = Sys.getenv("USERNAME"))
  new_fish_event = new_fish_event %>%
    mutate(depth_range = if_else(depth_range == "NA: NA", "", depth_range))
  return(new_fish_event)
})

# Generate values to show in modal
output$fish_event_modal_insert_vals = renderDT({
  fish_event_modal_in_vals = fish_event_create() %>%
    select(species, fish_count, catch_result, ad_mark,
           depth_range, gear_type)
  # Generate table
  datatable(fish_event_modal_in_vals,
            colnames = c("Species", "Fish count", "Kept?", "Mark", "Depth range", "Gear"),
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
observeEvent(input$fish_add, {
  new_fish_event_vals = fish_event_create()
  existing_fish_event_vals = get_fish_encounter(pool, selected_interview_event_data()$survey_event_id)
  dup_fish_event_flag = dup_fish_event(new_fish_event_vals, existing_fish_event_vals)
  showModal(
    # Verify all fields have data...none can be blank
    tags$div(id = "fish_event_insert_modal",
             if ( is.na(new_fish_event_vals$species) |
                  is.na(new_fish_event_vals$fish_count) |
                  is.na(new_fish_event_vals$catch_result) |
                  is.na(new_fish_event_vals$ad_mark) |
                  is.na(new_fish_event_vals$depth_range) |
                  is.na(new_fish_event_vals$gear_type) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required in all fields highlighted in green"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( dup_fish_event_flag == TRUE ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Interview data already exists. Please change at least one value before proceeding."),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new fish data to the database?"),
                 fluidPage (
                   DT::DTOutput("fish_event_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_fish_event", "Insert fish data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
fish_event_insert_vals = reactive({
  new_fish_event_values = fish_event_create() %>%
    select(survey_event_id, species_id, fish_count,
           catch_result_type_id, adipose_clip_status_id,
           encounter_depth_range_id, encounter_gear_type_id,
           created_by)
  return(new_fish_event_values)
})

# Update DB and reload DT
observeEvent(input$insert_fish_event, {
  tryCatch({
    fish_event_insert(pool, fish_event_insert_vals())
    shinytoastr::toastr_success("New fish data was added")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_fish_event_insert_vals = get_fish_encounter(pool, selected_interview_event_data()$survey_event_id) %>%
    select(species, fish_count, catch_result, ad_mark, depth_range, gear_type,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(fish_encounter_dt_proxy, post_fish_event_insert_vals)
})

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
fish_event_edit = reactive({
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  req(input$fish_encounters_rows_selected)
  # fish_encounter_id
  fish_encounter_id_input = selected_fish_encounter_data()$fish_encounter_id
  # Species
  event_species_input = input$event_species_select
  if (event_species_input == "" ) {
    event_species_id = NA_character_
  } else {
    event_species_vals = get_event_species(pool)
    event_species_id = event_species_vals %>%
      filter(event_species == event_species_input) %>%
      pull(event_species_id)
  }
  # Catch result
  catch_result_input = input$catch_result_select
  if (catch_result_input == "" ) {
    catch_result_type_id = NA_character_
  } else {
    catch_result_vals = get_catch_result(pool)
    catch_result_type_id = catch_result_vals %>%
      filter(catch_result == catch_result_input) %>%
      pull(catch_result_type_id)
  }
  # AD mark
  ad_mark_input = input$ad_mark_select
  if ( ad_mark_input == "" ) {
    adipose_clip_status_id = NA_character_
  } else {
    ad_mark_vals = get_ad_mark(pool)
    adipose_clip_status_id = ad_mark_vals %>%
      filter(ad_mark == ad_mark_input) %>%
      pull(adipose_clip_status_id)
  }
  # Depth range
  depth_range_input = input$depth_range_select
  if ( depth_range_input == "" ) {
    encounter_depth_range_id = NA_character_
  } else {
    depth_range_vals = get_depth_range(pool)
    encounter_depth_range_id = depth_range_vals %>%
      filter(depth_range == depth_range_input) %>%
      pull(encounter_depth_range_id)
  }
  # Gear type
  gear_type_input = input$gear_type_select
  if ( gear_type_input == "" ) {
    encounter_gear_type_id = NA_character_
  } else {
    gear_type_vals = get_gear_type(pool)
    encounter_gear_type_id = gear_type_vals %>%
      filter(gear_type == gear_type_input) %>%
      pull(encounter_gear_type_id)
  }
  edit_fish_event = tibble(fish_encounter_id = fish_encounter_id_input,
                           species = event_species_input,
                           species_id = event_species_id,
                           fish_count = input$fish_count_input,
                           catch_result = catch_result_input,
                           catch_result_type_id = catch_result_type_id,
                           ad_mark = ad_mark_input,
                           adipose_clip_status_id = adipose_clip_status_id,
                           depth_range = depth_range_input,
                           encounter_depth_range_id = encounter_depth_range_id,
                           gear_type = gear_type_input,
                           encounter_gear_type_id = encounter_gear_type_id)
  edit_fish_event = edit_fish_event %>%
    mutate(depth_range = if_else(depth_range == "NA: NA", "", depth_range))
  return(edit_fish_event)
})

# Generate values to show in modal
output$fish_event_modal_update_vals = renderDT({
  fish_event_modal_up_vals = fish_event_edit() %>%
    select(species, fish_count, catch_result, ad_mark,
           depth_range, gear_type)
  # Generate table
  datatable(fish_event_modal_up_vals,
            colnames = c("Species", "Fish count", "Kept?", "Mark", "Depth range", "Gear"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$fish_edit, {
  new_fish_event_vals = fish_event_edit() %>%
    mutate(species = as.character(species)) %>%
    mutate(fish_count = as.integer(fish_count)) %>%
    mutate(catch_result = as.character(catch_result)) %>%
    mutate(ad_mark = as.character(ad_mark)) %>%
    mutate(depth_range = as.character(depth_range)) %>%
    mutate(gear_type = as.character(gear_type)) %>%
    select(species, fish_count, catch_result, ad_mark,
           depth_range, gear_type)
  old_fish_event_vals = selected_fish_encounter_data() %>%
    mutate(species = as.character(species)) %>%
    mutate(fish_count = as.integer(fish_count)) %>%
    mutate(catch_result = as.character(catch_result)) %>%
    mutate(ad_mark = as.character(ad_mark)) %>%
    mutate(depth_range = as.character(depth_range)) %>%
    mutate(gear_type = as.character(gear_type)) %>%
    select(species, fish_count, catch_result, ad_mark,
           depth_range, gear_type)
  showModal(
    tags$div(id = "fish_event_update_modal",
             if ( isTRUE(all_equal(old_fish_event_vals, new_fish_event_vals)) ) {
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
                 title = "Update fish data to these new values?",
                 fluidPage (
                   DT::DTOutput("fish_event_modal_update_vals"),
                   br(),
                   br(),
                   actionButton("save_fish_event_edits","Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})


# Update DB and reload DT
observeEvent(input$save_fish_event_edits, {
  tryCatch({
    fish_event_update(pool, fish_event_edit())
    shinytoastr::toastr_success("Fish data was edited")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_fish_event_edit_vals = get_fish_encounter(pool, selected_interview_event_data()$survey_event_id) %>%
    select(species, fish_count, catch_result, ad_mark, depth_range, gear_type,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(fish_encounter_dt_proxy, post_fish_event_edit_vals)
})

#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$fish_event_modal_delete_vals = renderDT({
  fish_event_modal_del_id = selected_fish_encounter_data()$fish_encounter_id
  fish_event_modal_del_vals = get_fish_encounter(pool, selected_interview_event_data()$survey_event_id) %>%
    filter(fish_encounter_id == fish_event_modal_del_id) %>%
    select(species, fish_count, catch_result, ad_mark, depth_range, gear_type)
  # Generate table
  datatable(fish_event_modal_del_vals,
            colnames = c("Species", "Fish count", "Kept?", "Mark", "Depth range", "Gear"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$fish_delete, {
  req(input$fish_encounters_rows_selected)
  fish_encounter_id = selected_fish_encounter_data()$fish_encounter_id
  fish_event_dependencies = get_fish_event_dependencies(pool, fish_encounter_id)
  table_names = paste0(names(fish_event_dependencies), collapse = ", ")
  #table_names = gsub("survey_event", "species_data", table_names)
  showModal(
    tags$div(id = "fish_event_delete_modal",
             if ( ncol(fish_event_dependencies) > 0L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 glue("Please delete associated fish data from the following tables first: {table_names}"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Are you sure you want to delete this fish data from the database?",
                 fluidPage (
                   DT::DTOutput("fish_event_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_fish_event", "Delete fish data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_fish_event, {
  tryCatch({
    fish_event_delete( pool, selected_fish_encounter_data() )
    shinytoastr::toastr_success("Fish data was deleted")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  fish_events_after_delete = get_fish_encounter(pool, selected_interview_event_data()$survey_event_id) %>%
    select(species, fish_count, catch_result, ad_mark, depth_range, gear_type,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(fish_encounter_dt_proxy, fish_events_after_delete)
})

