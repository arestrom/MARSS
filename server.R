#=========================================================
# Top-level server code
#=========================================================

# Create the Shiny server
server = function(input, output, session) {
  source("crc_site/crc_site_srv.R", local = TRUE)
  source("survey/survey_srv.R", local = TRUE)
  source("survey_event/survey_event_srv.R", local = TRUE)
  source("fish_encounter/fish_encounter_srv.R", local = TRUE)
  source("individual_fish/individual_fish_srv.R", local = TRUE)
  source("sample_site/sample_site_srv.R", local = TRUE)
  source("connect/connect_srv.R", local = TRUE)

  # Go to connect tab if pool is invalid
  if ( valid_connection[1] == FALSE ) {
    failed_reason = trimws(attr(valid_connection, "reason"))
    updateTabItems(session, "tabs", "connect")
    showModal(
      tags$div(id = "no_credentials_modal",
               modalDialog (
                 size = "l",
                 title = "No database connection!",
                 withTags({
                   div(class="header", checked=NA,
                       p(glue("If this is the first time running the application ",
                              "you will need to enter your connection parameters ",
                              "below to store the values.")),
                       HTML("<font color=#660033><strong>IMPORTANT!</strong><font color=#000080>"),
                       p(glue("The connection failed due to: '{failed_reason}'. ",
                              "Please enter your database credentials below. This ",
                              "should only need to be done once unless your ",
                              "password lapses, or there are changes to the ",
                              "server. If you are still unable to connect after ",
                              "attempting to set the parameters please contact ",
                              "the database administrator to verify your credentials. ",
                              "Click anywhere outside this box to close the popup."))
                   )
                 }),
                 easyClose = TRUE,
                 footer = NULL
               )
      )
    )
  }

  # # Close the R session when exiting the browser...for standalone
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
}
