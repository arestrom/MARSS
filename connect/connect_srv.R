#==============================================================
# Set up connection...use server as source of truth for pwd
#==============================================================

# Initialize rv to indicate login is valid
connection_check = reactiveValues(con_check = NULL)

# Tasks to run first when login button is clicked
observeEvent(input$test_connection, {
  # Check to see if user can connect
  con_valid = DBI::dbCanConnect(RPostgres::Postgres(),
                                host = input$database_host,
                                port = input$database_port,
                                user = input$user_name,
                                password = input$password,
                                dbname = input$database_name)
  if (con_valid == TRUE ) {
    connection_check$con_check = TRUE
  } else {
    test_failed_reason = trimws(attr(con_valid, "reason"))
    showModal(
      tags$div(id = "test_failed_modal",
               modalDialog (
                 size = "m",
                 title = "Test failed",
                 withTags({
                   div(class="header", checked = NA,
                       p(glue("Please try again, or contact the database administrator ",
                              "to obtain the current connection parameters.")),
                       HTML("<font color=#660033><strong>Error message:</strong><font color=#000080>"),
                       p(test_failed_reason)
                   )
                 }),
                 easyClose = TRUE,
                 footer = NULL
               )
      )
    )
    updateTextInput(session, "password", value = "")
    updateTextInput(session, "database_host", value = "")
    connection_check$con_check = FALSE
  }
}, priority = 9999)

# Tasks to run second when login button is clicked
observeEvent(input$test_connection, {
  req(!is.null(connection_check$con_check))
  if ( connection_check$con_check == TRUE ) {
    write_credentials(dbname = input$database_name,
                      host = input$database_host,
                      port = input$database_port,
                      password = input$password)
    showModal(
      tags$div(id = "write_credentials_modal",
               modalDialog (
                 size = "m",
                 title = "Success!",
                 withTags({
                   div(class="header", checked = NA,
                       p(glue("Your connection parameters have been encrypted ",
                              "and written to the Windows Credential Manager.")),
                       HTML("<font color=#1d3f87><strong>IMPORTANT!</strong>"),
                       p(glue("Please close and restart the application to ",
                              "enable the connection!"))
                   )
                 }),
                 easyClose = TRUE,
                 footer = NULL
               )
      )
    )
  }
}, priority = -1)
