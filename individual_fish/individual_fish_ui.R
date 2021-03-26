#=============================================================
# HTML content definition of individual fish accordion
#=============================================================

# Define the survey data content
individual_fish_ui = tags$div(
  actionButton(inputId = "ind_fish_add", label = "New", class = "new_button"),
  actionButton(inputId = "ind_fish_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "ind_fish_delete", label = "Delete", class = "delete_button"),
  br(),
  br(),
  numericInput(inputId = "length_input", label = "Fork Length (cm)", value = NULL,
               min = 0, step = 1, width = "150px"),
  numericInput(inputId = "weight_input", label = "Weight (kg)", value = NULL,
               min = 0, step = 1, width = "100px"),
  uiOutput("sex_select", inline = TRUE),
  textInput(inputId = "cwt_label_input", label = "CWT label", width = "125px"),
  uiOutput("head_taken_select", inline = TRUE),
  br(),
  br(),
  br(),
  DT::DTOutput("individual_fishes")
)
