
# Server side inputs
output$survey_site_select = renderUI({
  req(valid_connection == TRUE)
  survey_site_list = get_all_sites(pool)$survey_site
  selectizeInput("survey_site_select", label = "Survey site",
                 choices = survey_site_list, selected = NULL,
                 width = "325px")
})

output$sampler_select = renderUI({
  req(valid_connection == TRUE)
  sampler_list = get_samplers(pool)$sampler_name
  selectizeInput("sampler_select", label = "Sampler name",
                 choices = sampler_list, selected = NULL,
                 width = "225px")
})

output$survey_design_select = renderUI({
  req(valid_connection == TRUE)
  design_list = get_survey_design(pool)$survey_design
  selectizeInput("survey_design_select", label = "Survey design",
                 choices = design_list, selected = NULL,
                 width = "175px")
})

output$any_effort_select = renderUI({
  req(valid_connection == TRUE)
  selectizeInput("any_effort_select", label = "Any effort?",
                 choices = c("Yes", "No"), selected = "Yes",
                 width = "100px")
})

# Get all creel sites in selected catch areas for selected years
when_where_surveys = reactive({
  req(site_sampler_info())
  req(site_sampler_dates())
  req(input$site_sampler_date_select)
  input_site_codes = substr(input$site_select, 1, 4)
  input_sampler_names = input$site_sampler_select
  input_code_dates = input$site_sampler_date_select
  filtered_survey_list = site_sampler_dates() %>%
    filter(site_code %in% input_site_codes) %>%
    filter(sampler_name %in% input_sampler_names) %>%
    filter(code_date %in% input_code_dates) %>%
    arrange(creel_site, as.Date(survey_date), sampler_name)
  return(filtered_survey_list)
})

# Pull out sites as a text string
survey_ids = reactive({
  req(when_where_surveys())
  survey_input = when_where_surveys()$survey_id
  # Account for cases where no locations were selected
  if (survey_input[[1]] == "" ) {
    survey_ids = get_uuid(1L)
  } else {
    survey_ids = when_where_surveys()$survey_id
  }
  survey_ids = paste0(paste0("'", survey_ids, "'"), collapse = ", ")
  return(survey_ids)
})

# Primary DT datatable for database
output$surveys = renderDT({
  req(survey_ids())
  sites = paste0(input$site_select, collapse = ", ")
  dates = substr(input$site_sampler_date_select, 7, 17)
  dates = paste0(dates, collapse = ", ")
  survey_title = glue("Surveys for {sites} on {dates}")
  survey_data = get_surveys(pool, survey_ids()) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_date = survey_date_dt, survey_site, sampler_name,
           start_time, end_time, survey_design, any_effort,
           survey_comment, created_dt, created_by,
           modified_dt, modified_by)
  # Generate table
  datatable(survey_data,
            colnames = c("Survey date", "Survey site", "Sampler name", "Start time",
                         "End time", "Survey design", "Any effort?", "Survey comment",
                         "Create DT", "Create By", "Mod DT", "Mod By"),
            selection = list(mode = 'single'),
            extensions = 'Buttons',
            options = list(dom = 'Blftp',
                           pageLength = 10,
                           lengthMenu = c(5, 10, 20, 40, 60, 100, 500),
                           scrollX = T,
                           buttons = c('excel', 'print'),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(survey_title))))
})

# Create surveys DT proxy object
survey_dt_proxy = dataTableProxy(outputId = "surveys")

# Set row selection to NULL if tab changes
observeEvent(input$tabs, {
  if (input$tabs == "when_where") {
    selectRows(survey_dt_proxy, NULL)
  }
})

#========================================================
# Collect survey values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_survey_data = reactive({
  req(input$surveys_rows_selected)
  surveys = get_surveys(pool, survey_ids())
  survey_row = input$surveys_rows_selected
  selected_survey = tibble(survey_id = surveys$survey_id[survey_row],
                           survey_date = surveys$survey_date[survey_row],
                           survey_site = surveys$survey_site[survey_row],
                           sampler_name = surveys$sampler_name[survey_row],
                           start_time = surveys$start_time[survey_row],
                           end_time = surveys$end_time[survey_row],
                           survey_design = surveys$survey_design[survey_row],
                           any_effort = surveys$any_effort[survey_row],
                           survey_comment = surveys$survey_comment[survey_row],
                           created_date = surveys$created_date[survey_row],
                           created_by = surveys$created_by[survey_row],
                           modified_date = surveys$modified_date[survey_row],
                           modified_by = surveys$modified_by[survey_row])
  return(selected_survey)
})

#========================================================
# Update survey select inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$surveys_rows_selected, {
  req(input$surveys_rows_selected)
  ssdat = selected_survey_data()
  updateDateInput(session, "survey_date_input", value = ssdat$survey_date)
  updateSelectizeInput(session, "survey_site_select", selected = ssdat$survey_site)
  updateSelectizeInput(session, "sampler_select", selected = ssdat$sampler_name)
  updateTimeInput(session, "start_time_select", value = ssdat$start_time)
  updateTimeInput(session, "end_time_select", value = ssdat$end_time)
  updateSelectizeInput(session, "survey_design_select", selected = ssdat$survey_design)
  updateSelectizeInput(session, "any_effort_select", selected = ssdat$any_effort)
  updateTextAreaInput(session, "survey_comment_input", value = ssdat$survey_comment)
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
survey_create = reactive({
  req(input$tabs == "data_edits")
  # Survey date
  survey_date_input = input$survey_date_input
  # Survey site
  survey_site_input = input$survey_site_select
  if (survey_site_input == "" ) {
    survey_site_id = NA
  } else {
    survey_site_vals = get_all_sites(pool)
    survey_site_id = survey_site_vals %>%
      filter(survey_site == survey_site_input) %>%
      pull(location_id)
  }
  # Sampler
  sampler_input = input$sampler_select
  if (sampler_input == "" ) {
    sampler_id = NA
  } else {
    sampler_vals = get_samplers(pool)
    sampler_id = sampler_vals %>%
      filter(sampler_name == sampler_input) %>%
      pull(sampler_id)
  }
  # Survey design
  survey_design_input = input$survey_design_select
  if (survey_design_input == "" ) {
    survey_design_id = NA
  } else {
    survey_design_vals = get_survey_design(pool)
    survey_design_id = survey_design_vals %>%
      filter(survey_design == survey_design_input) %>%
      pull(survey_design_type_id)
  }
  # Time values
  start_time = format(input$start_time_select)
  if (nchar(start_time) < 16) { start_time = NA_character_ }
  start_time = as.POSIXct(start_time)
  end_time = format(input$end_time_select)
  if (nchar(end_time) < 16) { end_time = NA_character_ }
  end_time = as.POSIXct(end_time)
  # No effort indicator
  any_effort_input = input$any_effort_select
  # Collect values
  new_survey = tibble(survey_date = survey_date_input,
                      survey_site = survey_site_input,
                      survey_site_id = survey_site_id,
                      sampler_name = sampler_input,
                      sampler_id = sampler_id,
                      survey_design = survey_design_input,
                      survey_design_id = survey_design_id,
                      start_time = start_time,
                      end_time = end_time,
                      any_effort = any_effort_input,
                      survey_comment = input$survey_comment_input,
                      created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                      created_by = Sys.getenv("USERNAME"))
  new_survey = new_survey %>%
    mutate(survey_comment = trimws(survey_comment)) %>%
    mutate(survey_comment = if_else(is.na(survey_comment) | survey_comment == "",
                                    NA_character_, survey_comment))
  return(new_survey)
})

# Generate values to show in modal
output$survey_modal_insert_vals = renderDT({
  survey_modal_in_vals = survey_create() %>%
    mutate(start_time = format(start_time, "%H:%M")) %>%
    mutate(end_time = format(end_time, "%H:%M")) %>%
    select(survey_date, survey_site, sampler_name, survey_design,
           start_time, end_time, any_effort, survey_comment)
  # Generate table
  datatable(survey_modal_in_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Generate modal
observeEvent(input$survey_add, {
  new_survey_vals = survey_create() %>%
    mutate(survey_date = format(survey_date)) %>%
    mutate(start_time = format(start_time, "%H:%M")) %>%
    mutate(end_time = format(end_time, "%H:%M")) %>%
    mutate(survey_design = if_else(survey_design == "", NA_character_, survey_design)) %>%
    select(survey_date, survey_site, sampler_name, start_time, end_time, any_effort)
  existing_survey_vals = get_surveys(pool, survey_ids()) %>%
    mutate(survey_date = format(survey_date)) %>%
    mutate(start_time = format(start_time, "%H:%M")) %>%
    mutate(end_time = format(end_time, "%H:%M")) %>%
    select(survey_date, survey_site, sampler_name, start_time, end_time, any_effort)
  dup_flag = dup_survey(pool, new_survey_vals, existing_survey_vals)
  showModal(
    # Verify required fields have values
    tags$div(id = "survey_insert_modal",
             if ( is.na(new_survey_vals$survey_date) |
                  is.na(new_survey_vals$survey_site) |
                  is.na(new_survey_vals$sampler_name) |
                  is.na(new_survey_vals$start_time) |
                  is.na(new_survey_vals$end_time) |
                  is.na(new_survey_vals$any_effort) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("All fields except survey design and survey comment are mandatory"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Verify survey is not already in database
             } else if ( dup_flag == TRUE ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Survey already exists. Please change at least one of the required values before proceeding." ),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( nrow(new_survey_vals) > 1L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Duplicate rows of survey data are being created" ),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new survey to the database?"),
                 fluidPage (
                   DT::DTOutput("survey_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_survey", "Insert survey")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values that will actually be inserted
survey_insert_vals = reactive({
  new_values = survey_create() %>%
    mutate(start_datetime = case_when(
      is.na(start_time) ~ as.POSIXct(NA),
      !is.na(start_time) ~ as.POSIXct(paste0(format(survey_date), " ", format(start_time, "%H:%M")),
                                                      tz = "America/Los_Angeles"))) %>%
    mutate(start_datetime = with_tz(start_datetime, tzone = "UTC")) %>%
    mutate(end_datetime = case_when(
      is.na(end_time) ~ as.POSIXct(NA),
      !is.na(end_time) ~ as.POSIXct(paste0(format(survey_date), " ", format(end_time, "%H:%M")),
                                      tz = "America/Los_Angeles"))) %>%
    mutate(end_datetime = with_tz(end_datetime, tzone = "UTC")) %>%
    mutate(survey_datetime = as.POSIXct(format(survey_date), tz = "America/Los_Angeles")) %>%
    mutate(survey_datetime = with_tz(survey_datetime, tzone = "UTC")) %>%
    mutate(no_effort_indicator = if_else(any_effort == "No", 1L, 0L)) %>%
    select(survey_design_id, survey_site_id, survey_datetime, start_datetime,
           end_datetime, no_effort_indicator, survey_comment, created_by,
           sampler_id)
  return(new_values)
})

# Update DB and reload DT
observeEvent(input$insert_survey, {
  tryCatch({
    survey_insert(pool, survey_insert_vals())
    shinytoastr::toastr_success("New survey was added")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_insert_vals = get_surveys(pool, survey_ids()) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_date = survey_date_dt, survey_site, sampler_name,
           start_time, end_time, survey_design, any_effort,
           survey_comment, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(survey_dt_proxy, post_insert_vals)
})

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for edit actions
survey_edit = reactive({
  req(input$surveys_rows_selected)
  # Survey date
  survey_date_input = input$survey_date_input
  # Survey design type
  survey_design_input = input$survey_design_select
  if ( survey_design_input == "" ) {
    survey_design_type_id = NA
  } else {
    survey_design_vals = get_survey_design(pool)
    survey_design_type_id = survey_design_vals %>%
      filter(survey_design == survey_design_input) %>%
      pull(survey_design_type_id)
  }
  # Survey site
  survey_site_input = input$survey_site_select
  if (survey_site_input == "" ) {
    survey_site_id = NA
  } else {
    survey_site_vals = get_all_sites(pool)
    survey_site_id = survey_site_vals %>%
      filter(survey_site == survey_site_input) %>%
      pull(location_id)
  }
  # Sampler
  sampler_input = input$sampler_select
  if (sampler_input == "" ) {
    sampler_id = NA
  } else {
    sampler_vals = get_samplers(pool)
    sampler_id = sampler_vals %>%
      filter(sampler_name == sampler_input) %>%
      pull(sampler_id)
  }
  # Time values
  start_time = format(input$start_time_select)
  if (nchar(start_time) < 16) { start_time = NA_character_ }
  start_time = as.POSIXct(start_time)
  end_time = format(input$end_time_select)
  if (nchar(end_time) < 16) { end_time = NA_character_ }
  end_time = as.POSIXct(end_time)
  # No effort indicator
  any_effort_input = input$any_effort_select
  # Collect values
  edit_survey = tibble(survey_id = selected_survey_data()$survey_id,
                       survey_date = survey_date_input,
                       survey_site = survey_site_input,
                       location_id = survey_site_id,
                       sampler_name = sampler_input,
                       sampler_id = sampler_id,
                       survey_design = survey_design_input,
                       survey_design_type_id = survey_design_type_id,
                       start_time = start_time,
                       end_time = end_time,
                       any_effort = any_effort_input,
                       survey_comment = input$survey_comment_input,
                       modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
                       modified_by = Sys.getenv("USERNAME"))
  edit_survey = edit_survey %>%
    mutate(survey_comment = trimws(survey_comment)) %>%
    mutate(survey_comment = if_else(is.na(survey_comment) | survey_comment == "",
                                    NA_character_, survey_comment)) %>%
    mutate(start_datetime = case_when(
      is.na(start_time) ~ as.POSIXct(NA),
      !is.na(start_time) ~ as.POSIXct(paste0(format(survey_date), " ", format(start_time, "%H:%M")),
                                      tz = "America/Los_Angeles"))) %>%
    mutate(start_datetime = with_tz(start_datetime, tzone = "UTC")) %>%
    mutate(end_datetime = case_when(
      is.na(end_time) ~ as.POSIXct(NA),
      !is.na(end_time) ~ as.POSIXct(paste0(format(survey_date), " ", format(end_time, "%H:%M")),
                                    tz = "America/Los_Angeles"))) %>%
    mutate(end_datetime = with_tz(end_datetime, tzone = "UTC")) %>%
    mutate(survey_datetime = as.POSIXct(format(survey_date), tz = "America/Los_Angeles")) %>%
    mutate(survey_datetime = with_tz(survey_datetime, tzone = "UTC")) %>%
    mutate(no_effort_indicator = if_else(any_effort == "No", 1L, 0L)) %>%
    select(survey_id, survey_date, survey_datetime, survey_site, location_id,
           sampler_name, sampler_id, survey_design, survey_design_type_id, start_time,
           start_datetime, end_time, end_datetime, any_effort, no_effort_indicator,
           survey_comment)
  return(edit_survey)
})

# Generate values to show in modal
output$survey_modal_update_vals = renderDT({
  survey_modal_up_vals = survey_edit() %>%
    mutate(start_time = format(start_time, "%H:%M")) %>%
    mutate(end_time = format(end_time, "%H:%M")) %>%
    select(survey_date, survey_site, sampler_name, survey_design,
           start_time, end_time, any_effort, survey_comment)
  # Generate table
  datatable(survey_modal_up_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$survey_edit, {
  old_vals = selected_survey_data() %>%
    mutate(survey_date = format(survey_date)) %>%
    mutate(start_time = format(start_time, "%H:%M")) %>%
    mutate(end_time = format(end_time, "%H:%M")) %>%
    mutate(start_time = if_else(is.na(start_time), "00:00", start_time)) %>%
    mutate(end_time = if_else(is.na(end_time), "00:00", end_time)) %>%
    mutate(survey_design = if_else(survey_design == "", NA_character_, survey_design)) %>%
    select(survey_date, survey_site, sampler_name, start_time, end_time,
           survey_design, any_effort, survey_comment)
  new_vals = survey_edit() %>%
    mutate(survey_date = format(survey_date)) %>%
    mutate(start_time = format(start_time, "%H:%M")) %>%
    mutate(end_time = format(end_time, "%H:%M")) %>%
    mutate(survey_design = if_else(survey_design == "", NA_character_, survey_design)) %>%
    select(survey_date, survey_site, sampler_name, start_time, end_time,
           survey_design, any_effort, survey_comment)
  showModal(
    tags$div(id = "survey_update_modal",
             if ( !length(input$surveys_rows_selected) == 1 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to edit!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( isTRUE(all_equal(old_vals, new_vals)) ) {
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
                 title = "Update data for survey to these new values?",
                 fluidPage (
                   DT::DTOutput("survey_modal_update_vals"),
                   br(),
                   br(),
                   actionButton("save_survey_edits","Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$save_survey_edits, {
  tryCatch({
    survey_update(pool, survey_edit())
    shinytoastr::toastr_success("Survey was edited")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_survey_edit_vals = get_surveys(pool, survey_ids()) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_date = survey_date_dt, survey_site, sampler_name,
           start_time, end_time, survey_design, any_effort,
           survey_comment, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(survey_dt_proxy, post_survey_edit_vals)
})

#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$survey_modal_delete_vals = renderDT({
  survey_modal_del_id = selected_survey_data()$survey_id
  survey_modal_del_vals = get_surveys(pool, survey_ids()) %>%
    filter(survey_id == survey_modal_del_id) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_date = survey_date_dt, survey_site, sampler_name,
           start_time, end_time, survey_design, any_effort,
           survey_comment, created_dt, created_by,
           modified_dt, modified_by)
  # Generate table
  datatable(survey_modal_del_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$survey_delete, {
  survey_id = selected_survey_data()$survey_id
  survey_dependencies = get_survey_dependencies(pool, survey_id)
  table_names = paste0(names(survey_dependencies), collapse = ", ")
  showModal(
    tags$div(id = "survey_delete_modal",
             if ( length(survey_id) == 0 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to delete!" ),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( ncol(survey_dependencies) > 0L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 glue("Please delete associated survey data from the following tables first: {table_names}"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Are you sure you want to delete this survey from the database?",
                 fluidPage (
                   DT::DTOutput("survey_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_survey", "Delete survey")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_survey, {
  tryCatch({
    survey_delete(pool, selected_survey_data())
    shinytoastr::toastr_success("Survey was deleted")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  surveys_after_delete = get_surveys(pool, survey_ids()) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_date = survey_date_dt, survey_site, sampler_name,
           start_time, end_time, survey_design, any_effort,
           survey_comment, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(survey_dt_proxy, surveys_after_delete)
})

