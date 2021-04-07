#=========================================================
# HTML content definition of header accordian
#========================================================

# Define the survey data content
survey_ui = tags$div(
  actionButton(inputId = "survey_add", label = "New", class = "new_button"),
  actionButton(inputId = "survey_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "survey_delete", label = "Delete", class = "delete_button"),
  tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
        tooltip = glue("<span style='font-size:11px;'>",
                       "Shaded input boxes in all data entry screens below indicate ",
                       "required fields. Please click the 'Log out' button when ",
                       "ready to end your session!<span>")),
  br(),
  br(),
  dateInput(inputId = "survey_date_input", label = "Survey date", format = "mm/dd/yyyy", value = Sys.Date()),
  uiOutput("survey_site_select", inline = TRUE),
  uiOutput("sampler_select", inline = TRUE),
  timeInput(inputId = "start_time_select", "start_time", seconds = FALSE),
  timeInput(inputId = "end_time_select", "end_time", seconds = FALSE),
  uiOutput("survey_design_select", inline = TRUE),
  uiOutput("any_effort_select", inline = TRUE),
  textAreaInput(inputId = "survey_comment_input", label = "survey_comment", value = "",
                width = "300px", resize = "both"),
  br(),
  br(),
  br(),
  DT::DTOutput("surveys")  %>%
    shinycssloaders::withSpinner(., size = 0.5)
)

