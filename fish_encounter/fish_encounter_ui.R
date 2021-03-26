#=============================================================
# HTML content definition of the fish_encounter box
#=============================================================

# Define the survey data content
fish_encounter_ui = tags$div(
  actionButton(inputId = "fish_add", label = "New", class = "new_button"),
  actionButton(inputId = "fish_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "fish_delete", label = "Delete", class = "delete_button"),
  tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
        tooltip = glue("<span style='font-size:11px;'>",
                       "Shaded input boxes in all data entry screens below indicate ",
                       "required fields<span>")),
  br(),
  br(),
  uiOutput("event_species_select", inline = TRUE),
  numericInput(inputId = "fish_count_input", label = "Fish count", value = NA,
               min = 1, step = 1, width = "75px"),
  uiOutput("catch_result_select", inline = TRUE),
  uiOutput("ad_mark_select", inline = TRUE),
  uiOutput("depth_range_select", inline = TRUE),
  uiOutput("gear_type_select", inline = TRUE),
  br(),
  br(),
  br(),
  DT::DTOutput("fish_encounters") # %>%
  #shinycssloaders::withSpinner(., size = 0.5)
)
