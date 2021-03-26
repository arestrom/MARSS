#=============================================================
# HTML content definition of sample site interface
#=============================================================

# Define the survey data content
sample_site_ui = tags$div(
  actionButton(inputId = "sample_site_add", label = "New", class = "new_button"),
  actionButton(inputId = "sample_site_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "sample_site_delete", label = "Delete", class = "delete_button"),
  actionButton(inputId = "use_sample_site_map", label = "Use map", class = "map_button"),
  tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
        tooltip = glue("<span style='font-size:11px;'>",
                       "Click on the 'Use map' button to interactively select the latitude ",
                       "and longitude coordinates using a map interface.<span>")),
  br(),
  br(),
  textInput(inputId = "location_name_input", label = "Site name", width = "200px"),
  textInput(inputId = "location_code_input", label = "Site code", width = "125px"),
  numericInput(inputId = "sample_site_latitude_input", label = "Latitude", value = NULL,
               min = 45, max = 49, step = 0.001, width = "125px"),
  numericInput(inputId = "sample_site_longitude_input", label = "Longitude", value = NULL,
               min = -124, max = -116, step = 0.001, width = "125px"),
  uiOutput("active_select", inline = TRUE),
  airDatepickerInput(inputId = "active_date_input", label = "Active date",
                     dateFormat = "mm/dd/yyyy", width = "200px"),
  airDatepickerInput(inputId = "inactive_date_input", label = "Inactive date",
                     dateFormat = "mm/dd/yyyy", width = "200px"),
  textAreaInput(inputId = "inactive_reason_input", label = "Inactive reason", value = "",
                width = "300px", resize = "both"),
  textAreaInput(inputId = "sample_site_description_input", label = "Description", value = "",
                width = "300px", resize = "both"),
  textAreaInput(inputId = "sample_site_comment_input", label = "Comment", value = "",
                width = "300px", resize = "both"),
  br(),
  br(),
  br(),
  DT::DTOutput("sample_sites") %>%
    shinycssloaders::withSpinner(., size = 0.5)
)
