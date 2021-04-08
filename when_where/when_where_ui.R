#=========================================================
# HTML content definition of header page
#========================================================

# Define the survey data content
when_where_ui = tags$div(
  div(id = "rockfish_image", img(src = "copper_rockfish.png", width = "100%")),
  br(),
  br(),
  br(),
  uiOutput("year_select"),
  br(),
  uiOutput("month_select"),
  br(),
  uiOutput("crc_select"),
  br(),
  uiOutput("site_select"),
  br(),
  uiOutput("date_select")
)
