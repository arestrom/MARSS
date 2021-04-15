#=============================================================
# HTML content definition of survey_event accordion
#=============================================================

# Define the survey data content
survey_event_ui = tags$div(
  fluidRow(
    column(width = 4,
           actionButton(inputId = "interview_add", label = "New", class = "new_button"),
           actionButton(inputId = "interview_edit", label = "Edit", class = "edit_button"),
           actionButton(inputId = "interview_delete", label = "Delete", class = "delete_button"),
           tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
                 tooltip = glue("<span style='font-size:11px;'>",
                                "For any given interview there may be ",
                                "one or more set of fish and trip times.<span>")),
           br(),
           br(),
           numericInput(inputId = "interview_number_input", label = "Intvw Num", value = NA,
                        min = 1, step = 1, width = "75px"),
           uiOutput("catch_area_select", inline = TRUE),
           uiOutput("fishing_method_select", inline = TRUE),
           numericInput(inputId = "angler_count_input", label = "Num Anglrs", value = NA,
                        min = 1, step = 1, width = "75px"),
           uiOutput("cooperative_angler_select", inline = TRUE),
           br(),
           br(),
           br(),
           DT::DTOutput("interview_events")
    ),
    column(width = 5,
           actionButton(inputId = "dockside_add", label = "New", class = "new_button"),
           actionButton(inputId = "dockside_edit", label = "Edit", class = "edit_button"),
           actionButton(inputId = "dockside_delete", label = "Delete", class = "delete_button"),
           tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
                 tooltip = glue("<span style='font-size:11px;'>",
                                "Trip start and end times are normally only ",
                                "relevant for the aerial survey design. ",
                                "Although the paper forms typically only ask for ",
                                "trip times, these are actually the times when ",
                                "fishing started or ended. The sampling manual ",
                                "makes this clear.<span>")),
           br(),
           br(),
           airDatepickerInput(inputId = "trip_start_input", "Trip Start", value = NULL, timepicker = TRUE,
                              dateFormat = "mm/dd/yyyy", inline = FALSE, width = "100px", addon = "none",
                              timepickerOpts = timepickerOptions(
                                timeFormat = "hh:ii",
                                minutesStep = 1, hoursStep = 1)),
           airDatepickerInput(inputId = "fish_start_input", "Fish Start", value = NULL, timepicker = TRUE,
                              dateFormat = "mm/dd/yyyy", inline = FALSE, width = "100px", addon = "none",
                              timepickerOpts = timepickerOptions(
                                timeFormat = "hh:ii",
                                minutesStep = 1, hoursStep = 1)),
           airDatepickerInput(inputId = "fish_end_input", "Fish End", value = NULL, timepicker = TRUE,
                              dateFormat = "mm/dd/yyyy", inline = FALSE, width = "100px", addon = "none",
                              timepickerOpts = timepickerOptions(
                                timeFormat = "hh:ii",
                                minutesStep = 1, hoursStep = 1)),
           airDatepickerInput(inputId = "trip_end_input", "Trip End", value = NULL, timepicker = TRUE,
                              dateFormat = "mm/dd/yyyy", inline = FALSE, width = "100px", addon = "none",
                              timepickerOpts = timepickerOptions(
                                timeFormat = "hh:ii",
                                minutesStep = 1, hoursStep = 1)),
           numericInput(inputId = "no_license_input", label = "No license", value = NA,
                        min = 0, step = 1, width = "75px"),
           br(),
           br(),
           br(),
           DT::DTOutput("dockside_events")
    ),
    column(width = 3,
           actionButton(inputId = "target_add", label = "New", class = "new_button"),
           actionButton(inputId = "target_edit", label = "Edit", class = "edit_button"),
           actionButton(inputId = "target_delete", label = "Delete", class = "delete_button"),
           tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
                 tooltip = glue("<span style='font-size:11px;'>",
                                "Anglers may report one or more target species ",
                                "per interview. Sometimes they will switch for ",
                                "different segments of the same fishing trip.<span>")),
           br(),
           br(),
           uiOutput("target_species_select", inline = TRUE),
           br(),
           br(),
           br(),
           DT::DTOutput("target_events")
    )
  )
)
