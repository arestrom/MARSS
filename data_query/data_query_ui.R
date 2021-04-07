#=========================================================
# HTML content definition of sampler ui
#========================================================

# Define the survey data content
data_query_ui = tags$div(
  fluidRow(
    column(width = 12,
           br(),
           br(),
           airDatepickerInput(inputId = "survey_date_range",
                              label = "Date range for surveys",
                              range = TRUE,
                              dateFormat = "mm/dd/yyyy",
                              separator = " to ",
                              startView = "2017-01-01",
                              clearButton = TRUE,
                              width = "250px"),
           uiOutput("all_sites_select", inline = TRUE),
           uiOutput("all_samplers_select", inline = TRUE),
           br(),
           br(),
           br(),
           DT::DTOutput("surveys_query"))
  )
)
