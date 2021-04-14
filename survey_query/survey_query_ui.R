#=========================================================
# HTML content definition of sampler ui
#========================================================

# Define the survey data content
survey_query_ui = tags$div(
  fluidRow(
    column(width = 12,
           br(),
           br(),
           dateRangeInput(inputId = "survey_date_range",
                          label = "Date range for surveys",
                          start = "2012-07-01",
                          end = "2012-07-07",
                          format = "mm/dd/yyyy",
                          startview = "year",
                          width = "250px"),
           uiOutput("all_sites_select", inline = TRUE),
           uiOutput("all_samplers_select", inline = TRUE),
           br(),
           br(),
           br(),
           DT::DTOutput("surveys_query"))
  )
)
