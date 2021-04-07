#=========================================================
# HTML content definition of sampler ui
#========================================================

# Define the survey data content
sampler_ui = tags$div(
  fluidRow(
    column(width = 12,
           actionButton(inputId = "sampler_add", label = "New", class = "new_button"),
           actionButton(inputId = "sampler_edit", label = "Edit", class = "edit_button"),
           actionButton(inputId = "sampler_delete", label = "Delete", class = "delete_button"),
           tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
                 tooltip = glue("<span style='font-size:11px; font-weight:700;'>",
                                "Add or edit samplers.<span>")),
           br(),
           br(),
           textInput(inputId = "first_name_input", label = "first_name", width = "200px"),
           textInput(inputId = "last_name_input", label = "last_name", width = "200px"),
           uiOutput("all_sampler_select", inline = TRUE),
           br(),
           br(),
           br(),
           DT::DTOutput("samplers"))
  )
)
