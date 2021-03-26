# Function to write connection parameters to windows credential store
write_credentials = function(dbname, host, port, password) {
  if ( dbname == "sport_sampling" ) {
    keyring::key_set_with_value(service = "pg_sport_db",
                                username = NULL,
                                password = dbname,
                                keyring = NULL)
  } else if ( dbname == "ss_test" ) {
    keyring::key_set_with_value(service = "pg_sport_test_db",
                                username = NULL,
                                password = dbname,
                                keyring = NULL)
  } else {
    stop("You need to select a database.")
  }
  keyring::key_set_with_value(service = "pg_host_prod",
                              username = NULL,
                              password = host,
                              keyring = NULL)
  keyring::key_set_with_value(service = "pg_port_prod",
                              username = NULL,
                              password = port,
                              keyring = NULL)
  keyring::key_set_with_value(service = "pg_pwd_prod",
                              username = NULL,
                              password = password,
                              keyring = NULL)
}
