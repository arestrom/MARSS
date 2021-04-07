
output$all_sampler_select = renderUI({
  selectizeInput("all_sampler_select",
                 label = "Active?",
                 choices = c("Yes", "No"),
                 selected = "Yes",
                 width = "100px")
})

# # Primary DT datatable for samplers
# output$samplers = renderDT({
#   req(valid_connection == TRUE)
#   sampler_title = glue("All samplers currently listed in the database")
#   sampler_data = get_samplers(pool) %>%
#     select(first_name, last_name, active_indicator, created_dt, created_by,
#            modified_dt, modified_by)
#
#   # Generate table
#   datatable(sampler_data,
#             colnames = c("First name", "Last name", "Active?", "Create DT",
#                          "Create By", "Mod DT", "Mod By"),
#             selection = list(mode = 'single'),
#             options = list(dom = 'lftp',
#                            pageLength = 5,
#                            lengthMenu = c(5, 10, 50, 100, 500),
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
