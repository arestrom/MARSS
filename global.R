#==============================================================
# Application to edit historical baseline data
#
# Notes on install: R 4.0.2
#  1. Need to update shinydashboardPlus to latest breaking version.
#     Try on separate machine first, then upgrade here.
#
# Notes:
#  1. Do not use rownames = FALSE in DT. Data will not reload
#     when using replaceData() function.
#  2. To prune merged git branches:
#     a, Check which branches have been merged:       git branch --merged
#     b, Once branch has been deleted from remote:    git fetch -p            # To prune those no longer on remote
#     c, To delete branch from local:                 git branch -d <branch>
#
#  3. To size image for dash sidebar, expand canvas sufficiently using gimp. Then
#     set img(xxx.png, width = 100%) and it will stay centered unless web page
#     is resized excessively. Use css as for rockfish_image in current file.
#  4. To set security groups for postgis/postgres:
#     https://postgis.net/workshops/postgis-intro/security.html
#     Code located at: "PSS/sport_sampling/create_sport_sampling_roles.sql"
#
#
# ToDo:
#  1. Enable movable modals?
#  2. Add coordinates for all sites?
#  3. Add bird stuff?
#  4. Add ability to enter scale and age data later after
#     inital set of CWT with age data has been added? Can
#     then join by CWT label. This will handle 'scales taken'
#     question on paper form.
#  5. Compare baseline_data.css with marss.css to see where styling error came from.
#  6. For final version edit to make sure only SS is used...dump ss_test.
#  7. Add field to input 2-pole data at interview level?
#  8. Add field to input descending device data at interview level?
#  9. Leaked pooled object in compiled version.
# 10. Test connection button covered up.
#
# AS 2021-04-14
#==============================================================

# Load libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(shinyTime)
library(tippy)
library(glue)
library(tibble)
library(DBI)
library(RPostgres)
library(pool)
library(dplyr)
library(DT)
library(leaflet)
library(mapedit)
library(leaflet.extras)
library(sf)
library(lubridate)
library(uuid)
library(shinytoastr)
library(shinycssloaders)
library(stringi)
library(keyring)

# Read content definitions of data-entry screens
source("dashboard/dash_header.R")
source("dashboard/dash_leftsidebar.R")
source("when_where/when_where_ui.R")
source("when_where/when_where_global.R")
source("survey/survey_ui.R")
source("survey/survey_global.R")
source("survey_event/survey_event_ui.R")
source("survey_event/survey_event_global.R")
source("fish_encounter/fish_encounter_ui.R")
source("fish_encounter/fish_encounter_global.R")
source("individual_fish/individual_fish_ui.R")
source("individual_fish/individual_fish_global.R")
source("sample_site/sample_site_ui.R")
source("sample_site/sample_site_global.R")
source("sampler/sampler_ui.R")
source("sampler/sampler_global.R")
source("survey_query/survey_query_ui.R")
source("survey_query/survey_query_global.R")
source("connect/connect_ui.R")
source("connect/connect_global.R")

# Define functions =================================

# Generate a vector of Version 4 UUIDs (RFC 4122)
get_uuid = function(n = 1L) {
  if (!typeof(n) %in% c("double", "integer") ) {
    stop("n must be an integer or double")
  }
  uuid::UUIDgenerate(use.time = FALSE, n = n)
}

# Convert empty strings to NAs
set_na = function(x, na_value = "") {
  x[x == na_value] <- NA
  x
}

# Function to extract credentials from the windows credentials store
get_credentials = function(credential_label = NULL, keyring = NULL) {
  tryCatch({
    secret = key_get(service = credential_label,
                     keyring = keyring)
    return(secret)
  }, error = function(e) {
    msg = paste0("No credential was found with the label '", credential_label,
                 "'. Please check the spelling, or add the new credential.")
    cat("\n", msg, "\n\n")
  })
}

# Test credentials...return boolean
valid_connection = DBI::dbCanConnect(RPostgres::Postgres(),
                                     host = get_credentials("pg_host_prod"),
                                     port = get_credentials("pg_port_prod"),
                                     user = Sys.getenv("USERNAME"),
                                     password = get_credentials("pg_pwd_prod"),
                                     dbname = get_credentials("pg_sport_test_db"))

# Get pooled connection to AWS prod instance if credentials valid
if ( valid_connection == TRUE ) {
  pool = pool::dbPool(RPostgres::Postgres(),
                      dbname = get_credentials("pg_sport_test_db"),
                      host = get_credentials("pg_host_prod"),
                      port = get_credentials("pg_port_prod"),
                      user = Sys.getenv("USERNAME"),
                      password = get_credentials("pg_pwd_prod"))
}

# Define close function =============================================================

# Function to close pool
if ( valid_connection == TRUE ) {
  onStop(function() {
    poolClose(pool)
  })
}





