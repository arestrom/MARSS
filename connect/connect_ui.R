#=========================================================
# HTML content definition of database connection page
#========================================================

# Define the survey data content
connect_ui = tags$div(
  br(),
  br(),
  br(),
  div(id = "connect_panel",
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      wellPanel(
        tags$h3("Please enter credentials", class = "text-center",
                style = "padding-top: 0:"),
        br(),
        shinyjs::disabled(
          textInput(inputId = "user_name",
                    label = tagList(icon("user"), "User Name"),
                    value = Sys.getenv("USERNAME"),
                    width = "90%")
        ),
        tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
              tooltip = glue("<span style='font-size:11px;'>",
                             "Please contact the database administrator to obtain connection ",
                             "parameters such as password, host, and port. After you have ",
                             "successfully tested the connection you will need to close ",
                             "the application and restart. Your connection parameters will be ",
                             "encrypted and stored in the Windows Credential Manager after ",
                             "a successful test. You will not have to enter these values again ",
                             "unless your password expires, or changes are made to the ",
                             "database server.")),
        br(),
        br(),
        passwordInput(inputId = "password",
                      label = tagList(icon("unlock-alt"), "Password"),
                      width = "100%"),
        br(),
        br(),
        shinyjs::disabled(
        textInput(inputId = "database_name",
                  label = tagList(icon("database"), "Database name"),
                  value = "sport_sampling",
                  width = "100%")
        ),
        br(),
        br(),
        passwordInput(inputId = "database_host",
                      label = tagList(icon("database"), "Database host"),
                      width = "100%"),
        br(),
        br(),
        selectizeInput(inputId = "database_port",
                       label = tagList(icon("database"), "Database port"),
                       choices = c("5432", "5433"),
                       selected = "5432",
                       width = "100%"),
        br(),
        br(),
        div(
          style = "text-align: center;",
          actionButton(inputId = "test_connection",
                       label = "Test connection",
                       class = "btn-primary",
                       style = "color: white;")
        )
      )
  ),
  br(),
  br()
)
