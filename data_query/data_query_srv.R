
# All sites select
output$all_sites_select = renderUI({
  req(valid_connection == TRUE)
  survey_site_list = get_all_sites(pool)$survey_site
  pickerInput("survey_site_select", label = "Survey site",
              multiple = TRUE, choices = survey_site_list,
              selected = NULL, width = "325px",
              options = pickerOptions(actionsBox = TRUE))
})

# All sampler select
output$all_samplers_select = renderUI({
  req(valid_connection == TRUE)
  sampler_list = get_samplers(pool)$sampler_name
  pickerInput("sampler_select", label = "Sampler name",
              multiple = TRUE, choices = sampler_list,
              selected = NULL, width = "225px",
              options = pickerOptions(actionsBox = TRUE))
})

# # Primary DT datatable for samplers
# output$samplers = renderDT({
#   req(valid_connection == TRUE)
#   sampler_title = glue("All samplers currently listed in the database")
#   sampler_data = get_samplers(pool) %>%
#     select(first_name, last_name, active, created_dt, created_by,
#            modified_dt, modified_by)
#
#   # Generate table
#   datatable(sampler_data,
#             colnames = c("First name", "Last name", "Active?", "Create DT",
#                          "Create By", "Mod DT", "Mod By"),
#             selection = list(mode = 'single'),
#             options = list(dom = 'lftp',
#                            pageLength = 20,
#                            lengthMenu = c(20, 50, 100, 500, 2000),
#                            scrollX = T,
#                            initComplete = JS(
#                              "function(settings, json) {",
#                              "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
#                              "}")),
#             caption = htmltools::tags$caption(
#               style = 'caption-side: top; text-align: left; color: black; width: auto;',
#               htmltools::em(htmltools::strong(sampler_title))))
# })
#
# # Create surveys DT proxy object
# samplers_dt_proxy = dataTableProxy(outputId = "samplers")
#
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
