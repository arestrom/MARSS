# Function to write connection parameters to windows credential store
write_credentials = function(dbname, host, port, password) {
  keyring::key_set_with_value(service = "pg_sport_db",
                              username = NULL,
                              password = dbname,
                              keyring = NULL)
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
