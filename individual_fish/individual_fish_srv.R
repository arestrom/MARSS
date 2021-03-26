#========================================================
# Generate lut select ui's
#========================================================

output$sex_select = renderUI({
  req(valid_connection == TRUE)
  sex_list = get_sex(pool)$sex
  sex_list = c("", sex_list)
  selectizeInput("sex_select", label = "Sex",
                 choices = sex_list, selected = "Unknown",
                 width = "100px")
})

output$head_taken_select = renderUI({
  req(valid_connection == TRUE)
  head_taken_list = get_head_taken(pool)$head_taken
  head_taken_list = c("", head_taken_list)
  selectizeInput("head_taken_select", label = "Head Rcvd",
                 choices = head_taken_list, selected = "Yes",
                 width = "125px")
})

#========================================================
# Primary datatable for individual_fish
#========================================================

# Primary DT datatable for survey_intent
output$individual_fishes = renderDT({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  req(input$fish_encounters_rows_selected)
  req(!is.na(selected_fish_encounter_data()$fish_encounter_id))
  individual_fish_title = glue("Individual fish data for {selected_survey_data()$survey_site} on ",
                               "{selected_survey_data()$survey_date}")
  individual_fish_data = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    select(length_cm, weight_kg, sex, cwt_label, head_taken, created_dt, created_by,
           modified_dt, modified_by)

  # Generate table
  datatable(individual_fish_data,
            colnames = c("Fork Length (cm)", "Weight (kg)", "Sex", "CWT Label", "Head Rcvd",
                         "Create DT", "Create By", "Mod DT", "Mod By"),
            selection = list(mode = 'single'),
            options = list(dom = 'ltp',
                           pageLength = 5,
                           lengthMenu = c(1, 5, 10, 20, 100),
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(individual_fish_title))))
})

# Create surveys DT proxy object
individual_fish_dt_proxy = dataTableProxy(outputId = "individual_fishes")

#========================================================
# Collect encounter values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_individual_fish_data = reactive({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  req(input$fish_encounters_rows_selected)
  req(!is.na(selected_fish_encounter_data()$fish_encounter_id))
  individual_fish_data = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id)
  individual_fish_row = input$individual_fishes_rows_selected
  selected_individual_fish = tibble(individual_fish_id = individual_fish_data$individual_fish_id[individual_fish_row],
                                    fish_length_measurement_id = individual_fish_data$fish_length_measurement_id[individual_fish_row],
                                    length_cm = individual_fish_data$length_cm[individual_fish_row],
                                    weight_kg = individual_fish_data$weight_kg[individual_fish_row],
                                    sex = individual_fish_data$sex[individual_fish_row],
                                    cwt_label = individual_fish_data$cwt_label[individual_fish_row],
                                    head_taken = individual_fish_data$head_taken[individual_fish_row],
                                    created_date = individual_fish_data$created_date[individual_fish_row],
                                    created_by = individual_fish_data$created_by[individual_fish_row],
                                    modified_date = individual_fish_data$modified_date[individual_fish_row],
                                    modified_by = individual_fish_data$modified_by[individual_fish_row])
  return(selected_individual_fish)
})

#========================================================
# Update inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$individual_fishes_rows_selected, {
  sindat = selected_individual_fish_data()
  updateNumericInput(session, "length_input", value = sindat$length_cm)
  updateNumericInput(session, "weight_input", value = sindat$weight_kg)
  updateSelectizeInput(session, "sex_select", selected = sindat$sex)
  updateTextInput(session, "cwt_label_input", value = sindat$cwt_label)
  updateSelectizeInput(session, "head_taken_select", selected = sindat$head_taken)
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
individual_fish_create = reactive({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  req(input$fish_encounters_rows_selected)
  req(!is.na(selected_fish_encounter_data()$fish_encounter_id))
  # fish_encounter_id
  fish_encounter_id_input = selected_fish_encounter_data()$fish_encounter_id
  # Sex
  sex_input = input$sex_select
  if ( sex_input == "" ) {
    sex_id = NA
  } else {
    sex_vals = get_sex(pool)
    sex_id = sex_vals %>%
      filter(sex == sex_input) %>%
      pull(sex_id)
  }
  # Fish trauma
  head_taken_input = input$head_taken_select
  if ( head_taken_input == "" ) {
    cwt_head_taken_status_id = NA
  } else {
    head_taken_vals = get_head_taken(pool)
    cwt_head_taken_status_id = head_taken_vals %>%
      filter(head_taken == head_taken_input) %>%
      pull(cwt_head_taken_status_id)
  }
  new_individual_fish = tibble(fish_encounter_id = fish_encounter_id_input,
                               length_cm = input$length_input,
                               weight_kg = input$weight_input,
                               sex = sex_input,
                               sex_id = sex_id,
                               cwt_label = input$cwt_label_input,
                               head_taken = head_taken_input,
                               cwt_head_taken_status_id = cwt_head_taken_status_id,
                               created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                               created_by = Sys.getenv("USERNAME"))
  return(new_individual_fish)
})

# Generate values to show in modal
output$individual_fish_modal_insert_vals = renderDT({
  individual_fish_modal_in_vals = individual_fish_create() %>%
    select(length_cm, weight_kg, sex, cwt_label, head_taken)
  # Generate table
  datatable(individual_fish_modal_in_vals,
            colnames = c("Fork Length (cm)", "Weight (kg)", "Sex", "CWT Label", "Head Rcvd"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new individual fish data
observeEvent(input$ind_fish_add, {
  new_individual_fish_vals = individual_fish_create()
  existing_individual_fish_vals = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id)
  showModal(
    tags$div(id = "individual_fish_insert_modal",
             # Verify required fields have data...none can be blank
             if ( is.na(new_individual_fish_vals$length_cm) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Length is required"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( !is.na(new_individual_fish_vals$cwt_label) &
                        individual_fish_create()$cwt_label %in% existing_individual_fish_vals$cwt_label ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 glue("CWT Label ({individual_fish_create()$cwt_label}) already exists!"),
                 easyClose = TRUE,
                 footer = NULL
               )
            # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new individual fish data to the database?"),
                 fluidPage (
                   DT::DTOutput("individual_fish_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_individual_fish", "Insert data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
individual_fish_insert_vals = reactive({
  new_ind_fish_values = individual_fish_create() %>%
    mutate(cwt_head_taken_status_id = if_else(is.na(cwt_head_taken_status_id),
                                              "8f956ae4-ef6b-4a38-9bca-d8255948622a",
                                              cwt_head_taken_status_id)) %>%
    select(fish_encounter_id, length_cm, weight_kg, sex_id, cwt_label,
           cwt_head_taken_status_id, created_by)
  return(new_ind_fish_values)
})

# Update DB and reload DT
observeEvent(input$insert_individual_fish, {
  tryCatch({
    individual_fish_insert(pool, individual_fish_insert_vals())
    shinytoastr::toastr_success("New individual fish data was added")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_individual_fish_insert_vals = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    select(length_cm, weight_kg, sex, cwt_label, head_taken, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(individual_fish_dt_proxy, post_individual_fish_insert_vals)
})

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
individual_fish_edit = reactive({
  req(input$tabs == "data_edits")
  req(input$surveys_rows_selected)
  req(input$interview_events_rows_selected)
  req(input$fish_encounters_rows_selected)
  req(input$individual_fishes_rows_selected)
  req(!is.na(selected_individual_fish_data()$individual_fish_id))
  # Sex
  sex_input = input$sex_select
  if ( sex_input == "" ) {
    sex_id = NA
  } else {
    sex_vals = get_sex(pool)
    sex_id = sex_vals %>%
      filter(sex == sex_input) %>%
      pull(sex_id)
  }
  # Fish trauma
  head_taken_input = input$head_taken_select
  if ( head_taken_input == "" ) {
    cwt_head_taken_status_id = NA
  } else {
    head_taken_vals = get_head_taken(pool)
    cwt_head_taken_status_id = head_taken_vals %>%
      filter(head_taken == head_taken_input) %>%
      pull(cwt_head_taken_status_id)
  }
  edit_individual_fish = tibble(individual_fish_id = selected_individual_fish_data()$individual_fish_id,
                                length_cm = input$length_input,
                                weight_kg = input$weight_input,
                                sex = sex_input,
                                sex_id = sex_id,
                                cwt_label = input$cwt_label_input,
                                head_taken = head_taken_input,
                                cwt_head_taken_status_id = cwt_head_taken_status_id,
                                created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                                created_by = Sys.getenv("USERNAME"))
  edit_individual_fish = edit_individual_fish %>%
    mutate(cwt_label = if_else(is.na(cwt_label) | cwt_label == "", NA_character_, cwt_label)) %>%
  return(edit_individual_fish)
})

# Generate values to show in modal
output$individual_fish_modal_update_vals = renderDT({
  individual_fish_modal_up_vals = individual_fish_edit() %>%
    select(length_cm, weight_kg, sex, cwt_label, head_taken)
  # Generate table
  datatable(individual_fish_modal_up_vals,
            colnames = c("Fork Length (cm)", "Weight (kg)", "Sex", "CWT Label", "Head Rcvd"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$ind_fish_edit, {
  old_individual_fish_vals = selected_individual_fish_data() %>%
    mutate(length_cm = as.numeric(length_cm)) %>%
    mutate(weight_kg = as.numeric(weight_kg)) %>%
    mutate(sex = as.character(sex)) %>%
    mutate(cwt_label = as.character(cwt_label)) %>%
    mutate(head_taken = as.character(head_taken)) %>%
    select(length_cm, weight_kg, sex, cwt_label, head_taken)
  new_individual_fish_vals = individual_fish_edit() %>%
    mutate(length_cm = as.numeric(length_cm)) %>%
    mutate(weight_kg = as.numeric(weight_kg)) %>%
    mutate(sex = as.character(sex)) %>%
    mutate(cwt_label = as.character(cwt_label)) %>%
    mutate(head_taken = as.character(head_taken)) %>%
    select(length_cm, weight_kg, sex, cwt_label, head_taken)
  existing_individual_fish_vals = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id)
  showModal(
    tags$div(id = "individual_fish_update_modal",
             if ( !length(input$individual_fishes_rows_selected) == 1 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to edit!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( isTRUE(all_equal(old_individual_fish_vals, new_individual_fish_vals)) ) {
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
                 title = "Update individual fish data to these new values?",
                 fluidPage (
                   DT::DTOutput("individual_fish_modal_update_vals"),
                   br(),
                   br(),
                   actionButton("save_ind_fish_edits","Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$save_ind_fish_edits, {
  tryCatch({
    individual_fish_update(pool, individual_fish_edit())
    shinytoastr::toastr_success("Individual fish data was edited")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_individual_fish_edit_vals = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    select(length_cm, weight_kg, sex, cwt_label, head_taken, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(individual_fish_dt_proxy, post_individual_fish_edit_vals)
})

#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$individual_fish_modal_delete_vals = renderDT({
  req(selected_individual_fish_data()$individual_fish_id)
  individual_fish_modal_del_id = selected_individual_fish_data()$individual_fish_id
  individual_fish_modal_del_vals = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    filter(individual_fish_id == individual_fish_modal_del_id) %>%
    select(length_cm, weight_kg, sex, cwt_label, head_taken)
  # Generate table
  datatable(individual_fish_modal_del_vals,
            colnames = c("Fork Length (cm)", "Weight (kg)", "Sex", "CWT Label", "Head Rcvd"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$ind_fish_delete, {
  req(input$individual_fishes_rows_selected)
  individual_fish_id = selected_individual_fish_data()$individual_fish_id
  individual_fish_dependencies = get_individual_fish_dependencies(pool, individual_fish_id)
  table_names = paste0(names(individual_fish_dependencies), collapse = ", ")
  showModal(
    tags$div(id = "individual_fish_delete_modal",
             if ( ncol(individual_fish_dependencies) > 0L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 glue("Please delete associated individual fish data from the following tables first: {table_names}"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Are you sure you want to delete this set of individual fish data from the database?",
                 fluidPage (
                   DT::DTOutput("individual_fish_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_individual_fish", "Delete data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_individual_fish, {
  tryCatch({
    individual_fish_delete(pool, selected_individual_fish_data())
    shinytoastr::toastr_success("Sample data was deleted")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  individual_fish_after_delete = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    select(length_cm, weight_kg, sex, cwt_label, head_taken, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(individual_fish_dt_proxy, individual_fish_after_delete)
})
