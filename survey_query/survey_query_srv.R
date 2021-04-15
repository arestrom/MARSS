
# All sites select
output$all_sites_select = renderUI({
  req(valid_connection == TRUE)
  survey_site_list = get_survey_query_sites(pool)$survey_site
  pickerInput("all_sites_select",
              label = "Survey site(s)",
              multiple = TRUE,
              choices = survey_site_list,
              selected = survey_site_list[180],
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
              selected = survey_sampler_list[15],
              width = "225px",
              options = pickerOptions(
                actionsBox = TRUE
              )
  )
})

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
  sites = paste0(paste0("'", site_ids, "'"), collapse = ", ")
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
            colnames = c("Survey date", "Survey site", "Sampler name", "Start time",
                         "End time", "Survey type", "Any effort?", "Survey comment",
                         "Create DT", "Create By", "Mod DT", "Mod By"),
            selection = list(mode = 'single'),
            extensions = 'Buttons',
            options = list(dom = 'Blftp',
                           pageLength = 20,
                           lengthMenu = c(20, 50, 100, 500, 2000),
                           scrollX = T,
                           buttons = c('excel', 'print'),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(query_title))))
})

# Create surveys DT proxy object
surveys_query_dt_proxy = dataTableProxy(outputId = "surveys_query")

#========================================================
# Collect sampler values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_query_data = reactive({
  req(input$tabs == "survey_query")
  req(input$surveys_query_rows_selected)
  start_date = input$survey_date_range[1]
  end_date = input$survey_date_range[2]
  selected_sites = input$all_sites_select
  site_ids = get_survey_query_sites(pool) %>%
    filter(survey_site %in% selected_sites) %>%
    distinct() %>%
    pull(location_id)
  sites = paste0(paste0("'", site_ids, "'"), collapse = ", ")
  selected_samplers = input$all_samplers_select
  sampler_ids = get_survey_query_samplers(pool) %>%
    filter(sampler_name %in% selected_samplers) %>%
    distinct() %>%
    pull(sampler_id)
  samplers = paste0(paste0("'", sampler_ids, "'"), collapse = ", ")
  query_data = get_query_surveys(pool, start_date, end_date, sites, samplers)
  query_row = input$surveys_query_rows_selected
  selected_query_id = tibble(survey_id = query_data$survey_id[query_row],
                             survey_dt = query_data$survey_date_dt[query_row],
                             survey_site = query_data$survey_site[query_row])
  return(selected_query_id)
})

#========================================================
# Primary datatable for interview_events
#========================================================

# Primary DT datatable for survey_intent
output$query_events = renderDT({
  req(input$tabs == "survey_query")
  req(input$surveys_query_rows_selected)
  query_events_title = glue("Interview data for {selected_query_data()$survey_site} on ",
                               "{selected_query_data()$survey_dt}")
  query_events_data = get_query_event(pool, selected_query_data()$survey_id) %>%
    select(interview_number, catch_area, fishing_method, angler_count,
           cooperative_angler, trip_start, fish_start, fish_end, trip_end,
           created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(query_events_data,
            colnames = c("Intvw", "CRC", "Fish Meth", "Anglrs", "Cooperative?",
                         "Trip start", "Fish start", "Fish end", "Trip end",
                         "Create DT", "Create By", "Mod DT", "Mod By"),
            selection = list(mode = 'single'),
            extensions = 'Buttons',
            options = list(dom = 'Blftp',
                           pageLength = 20,
                           lengthMenu = c(20, 50, 100, 500),
                           scrollX = T,
                           buttons = c('excel', 'print'),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(query_events_title))))
})

# Create surveys DT proxy object
query_events_dt_proxy = dataTableProxy(outputId = "query_events")


