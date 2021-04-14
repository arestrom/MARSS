
# All sites select
output$all_sites_select = renderUI({
  req(valid_connection == TRUE)
  survey_site_list = get_survey_query_sites(pool)$survey_site
  pickerInput("all_sites_select",
              label = "Survey site(s)",
              multiple = TRUE,
              choices = survey_site_list,
              selected = survey_site_list[1],
              width = "325px",
              options = pickerOptions(
                actionsBox = TRUE
              )
  )
})

# All sampler select
output$all_samplers_select = renderUI({
  req(valid_connection == TRUE)
  survey_sampler_list = get_survey_query_samplers(pool)$sampler_name
  pickerInput("all_samplers_select",
              label = "Sampler name(s)",
              multiple = TRUE,
              choices = survey_sampler_list,
              selected = survey_sampler_list[1],
              width = "225px",
              options = pickerOptions(
                actionsBox = TRUE
              )
  )
})

# # All sampler select
# output$all_samplers_select = renderUI({
#   req(valid_connection == TRUE)
#   survey_sampler_list = get_survey_query_samplers(pool)$sampler_name
#   pickerInput("all_samplers_select",
#               label = "Sampler name(s)",
#               multiple = TRUE,
#               choices = survey_sampler_list,
#               selected = survey_sampler_list[1],
#               width = "225px",
#               options = list(pickerOptions(
#                 actionsBox = TRUE),
#                 "style-base" = "form-control", style = "")
#   )
# })

# Primary DT datatable for samplers
output$surveys_query = renderDT({
  req(valid_connection == TRUE)
  req(input$survey_date_range)
  req(input$all_sites_select)
  req(input$all_samplers_select)
  start_date = input$survey_date_range[1]
  end_date = input$survey_date_range[2]
  selected_sites = input$all_sites_select
  site_ids = get_survey_query_sites(pool) %>%
    filter(survey_site %in% selected_sites) %>%
    distinct() %>%
    pull(location_id)
  sites = paste0(paste0("'", site_ids, "'"), collapse = "' ")
  selected_samplers = input$all_samplers_select
  sampler_ids = get_survey_query_samplers(pool) %>%
    filter(sampler_name %in% selected_samplers) %>%
    distinct() %>%
    pull(sampler_id)
  samplers = paste0(paste0("'", sampler_ids, "'"), collapse = ", ")
  query_title = glue("All surveys for selected dates, sites, and samplers")
  query_data = get_query_surveys(pool, start_date, end_date, sites, samplers)  %>%
    select(survey_date_dt, survey_site, sampler_name, start_time_dt, end_time_dt,
           survey_design, any_effort, survey_comment, created_dt, created_by,
           modified_dt, modified_by)

  # Generate table
  datatable(query_data,
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

# #========================================================
# # Collect sampler values from selected row for later use
# #========================================================
#
# # Create reactive to collect input values for update and delete actions
# selected_sampler_data = reactive({
#   req(input$tabs == "sampler_info")
#   req(input$samplers_rows_selected)
#   samplers_data = get_samplers(pool)
#   samplers_row = input$samplers_rows_selected
#   selected_sampler = tibble(sampler_id = samplers_data$sampler_id[samplers_row],
#                             first_name = samplers_data$first_name[samplers_row],
#                             last_name = samplers_data$last_name[samplers_row],
#                             active = samplers_data$active[samplers_row])
#   return(selected_sampler)
# })
#
# #========================================================
# # Update sampler inputs to values in selected row
# #========================================================
#
# # Update all survey input values to values in selected row
# observeEvent(input$samplers_rows_selected, {
#   sampdat = selected_sampler_data()
#   updateTextInput(session, "first_name_input", value = sampdat$first_name)
#   updateTextInput(session, "last_name_input", value = sampdat$last_name)
#   updateSelectizeInput(session, "active_sampler_select", selected = sampdat$active)
# })
#
