
# Active sampler select
output$active_sampler_select = renderUI({
  selectizeInput("active_sampler_select",
                 label = "Active?",
                 choices = c("Yes", "No"),
                 selected = "Yes",
                 width = "100px")
})

# Primary DT datatable for samplers
output$samplers = renderDT({
  req(valid_connection == TRUE)
  sampler_title = glue("All samplers currently listed in the database")
  sampler_data = get_samplers(pool) %>%
    select(first_name, last_name, active, created_dt, created_by,
           modified_dt, modified_by)

  # Generate table
  datatable(sampler_data,
            colnames = c("First name", "Last name", "Active?", "Create DT",
                         "Create By", "Mod DT", "Mod By"),
            selection = list(mode = 'single'),
            options = list(dom = 'lftp',
                           pageLength = 20,
                           lengthMenu = c(20, 50, 100, 500, 2000),
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(sampler_title))))
})

# Create surveys DT proxy object
samplers_dt_proxy = dataTableProxy(outputId = "samplers")

#========================================================
# Collect sampler values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_sampler_data = reactive({
  req(input$tabs == "sampler_info")
  req(input$samplers_rows_selected)
  samplers_data = get_samplers(pool)
  samplers_row = input$samplers_rows_selected
  selected_sampler = tibble(sampler_id = samplers_data$sampler_id[samplers_row],
                            first_name = samplers_data$first_name[samplers_row],
                            last_name = samplers_data$last_name[samplers_row],
                            active = samplers_data$active[samplers_row])
  return(selected_sampler)
})

#========================================================
# Update sampler inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$samplers_rows_selected, {
  sampdat = selected_sampler_data()
  updateTextInput(session, "first_name_input", value = sampdat$first_name)
  updateTextInput(session, "last_name_input", value = sampdat$last_name)
  updateSelectizeInput(session, "active_sampler_select", selected = sampdat$active)
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
sampler_create = reactive({
  new_sampler = tibble(first_name = input$first_name_input,
                       last_name = input$last_name_input,
                       active = input$active_sampler_select,
                       created_by = Sys.getenv("USERNAME"))
  new_sampler = new_sampler %>%
    mutate(full_name = paste0(first_name, " ", last_name)) %>%
    select(first_name, last_name, full_name, active, created_by)
  return(new_sampler)
})

# Generate values to show in modal
output$sampler_modal_insert_vals = renderDT({
  sampler_modal_in_vals = sampler_create() %>%
    select(first_name, last_name, active)
  # Generate table
  datatable(sampler_modal_in_vals,
            colnames = c("First name", "Last name", "Active?"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new sampler
observeEvent(input$sampler_add, {
  new_sampler_vals = sampler_create()
  existing_samplers = get_samplers(pool) %>%
    select(full_name)
  showModal(
    # Verify all fields have data...non can not be blank
    tags$div(id = "sampler_insert_modal",
             if ( is.na(new_sampler_vals$first_name) |
                  new_sampler_vals$first_name == "" |
                  is.na(new_sampler_vals$last_name) |
                  new_sampler_vals$last_name == "" |
                  is.na(new_sampler_vals$active) |
                  new_sampler_vals$active == "" ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("All fields need to be filled in!"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Check if name already entered
             } else if (new_sampler_vals$full_name %in% existing_samplers$full_name) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Sampler name has already been entered, please enter a new name"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'm',
                 title = glue("Insert new sampler to the database?"),
                 fluidPage (
                   DT::DTOutput("sampler_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_sampler", "Insert sampler")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
sampler_insert_vals = reactive({
  sampler_values = sampler_create() %>%
    select(first_name, last_name, active, created_by)
  return(sampler_values)
})

# Update DB and reload DT
observeEvent(input$insert_sampler, {
  tryCatch({
    sampler_insert(pool, sampler_insert_vals())
    shinytoastr::toastr_success("New sampler was added")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_sampler_insert_vals = get_samplers(pool) %>%
    select(first_name, last_name, active, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(samplers_dt_proxy, post_sampler_insert_vals)
})

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input value for companions edit
sampler_edit = reactive({
  edit_sampler = tibble(sampler_id = selected_sampler_data()$sampler_id,
                        first_name = input$first_name_input,
                        last_name = input$last_name_input,
                        active = input$active_sampler_select,
                        modified_by = Sys.getenv("USERNAME"))
  return(edit_sampler)
})

# Generate values to show in modal
output$sampler_modal_update_vals = renderDT({
  sampler_modal_up_vals = sampler_edit() %>%
    select(first_name, last_name, active)
  # Generate table
  datatable(sampler_modal_up_vals,
            colnames = c("First name", "Last name", "Active?"),
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Observer for update modal
observeEvent(input$sampler_edit, {
  new_sampler_vals = sampler_edit() %>%
    mutate(first_name = as.character(first_name)) %>%
    mutate(last_name = as.character(last_name)) %>%
    mutate(active = as.character(active)) %>%
    select(first_name, last_name, active)
  old_sampler_vals = selected_sampler_data() %>%
    mutate(first_name = as.character(first_name)) %>%
    mutate(last_name = as.character(last_name)) %>%
    mutate(active = as.character(active)) %>%
    select(first_name, last_name, active)
  showModal(
    tags$div(id = "sampler_update_modal",
             if ( !length(input$samplers_rows_selected) == 1 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to edit!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( isTRUE(all_equal(old_sampler_vals, new_sampler_vals)) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please edit at least one value!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Update sampler data to these new values?",
                 fluidPage (
                   DT::DTOutput("sampler_modal_update_vals"),
                   br(),
                   br(),
                   actionButton("edit_sampler","Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$edit_sampler, {
  tryCatch({
    sampler_update(pool, sampler_edit())
    shinytoastr::toastr_success("Sampler data was edited")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_sampler_edit_vals = get_samplers(pool) %>%
    select(first_name, last_name, active, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(samplers_dt_proxy, post_sampler_edit_vals)
}, priority = 9999)

#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$sampler_modal_delete_vals = renderDT({
  sampler_modal_del_id = selected_sampler_data()$sampler_id
  sampler_modal_del_vals = get_samplers(pool) %>%
    filter(sampler_id == sampler_modal_del_id) %>%
    select(first_name, last_name, active)
  # Generate table
  datatable(sampler_modal_del_vals,
            colnames = c("First name", "Last name", "Active?"),
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
sampler_dependencies = reactive({
  sampler_id = selected_sampler_data()$sampler_id
  sampler_dep = get_sampler_dependencies(pool, sampler_id)
  return(sampler_dep)
})

observeEvent(input$sampler_delete, {
  sampler_deps = sampler_dependencies()
  table_names = paste0(paste0("'", names(sampler_deps), "'"), collapse = ", ")
  sampler_id = selected_sampler_data()$sampler_id
  showModal(
    tags$div(id = "sampler_delete_modal",
             if ( length(sampler_id) == 0 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to delete!" ),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( ncol(sampler_deps) > 0L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 glue("Please delete associated sampler data in the following tables first: {table_names}"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Are you sure you want to delete this sampler from the database?",
                 fluidPage (
                   DT::DTOutput("sampler_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_sampler", "Delete sampler")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Delete sampler and reload DT
observeEvent(input$delete_sampler, {
  tryCatch({
    sampler_delete(pool, selected_sampler_data())
    shinytoastr::toastr_success("Sampler was deleted")
  }, error = function(e) {
    shinytoastr::toastr_error(title = "Database error", conditionMessage(e))
  })
  removeModal()
  post_sampler_delete_vals = get_samplers(pool) %>%
    select(first_name, last_name, active, created_dt, created_by,
           modified_dt, modified_by)
  replaceData(samplers_dt_proxy, post_sampler_delete_vals)
})


