#=========================================================
# HTML content definition of header page
#========================================================

# Define the survey data content
when_where_ui = tags$div(
  div(id = "rockfish_image", img(src = "copper_rockfish.png", width = "100%")),
  br(),
  br(),
  br(),
  dateRangeInput(inputId = "when_date_range",
                 label = "Date range for surveys",
                 start = "2017-01-01",
                 end = "2017-01-31",
                 format = "mm/dd/yyyy",
                 startview = "year",
                 width = "250px"),
  br(),
  br(),
  uiOutput("site_select"),
  br(),
  uiOutput("site_sampler_select"),
  br(),
  uiOutput("site_sampler_date_select")
)
