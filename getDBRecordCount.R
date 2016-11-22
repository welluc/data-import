getDBRecordCount <- function(
  w_host, 
  w_port, 
  w_servicename, 
  w_tablename, 
  w_username, w_password
) {
  require(ROracle)
  
  ########################################
  # Connect to Database:
  ########################################
  
  tempdrv <- dbDriver('Oracle')
  
  connectstring <- paste(
    '(DESCRIPTION=',
    '(ADDRESS=(PROTOCOL=tcp)(HOST=', w_host, ')(PORT=', w_port, '))',
    '(CONNECT_DATA=(SERVICE_NAME=', w_servicename, ')))', sep = '')
  
  # connect to database
  con <- dbConnect(tempdrv, username = w_username, password = w_password,
                   dbname = connectstring)
  
  ########################################
  # Count Number of Records:
  ########################################
  
  numrecords <- as.integer(dbGetQuery(con, 
                           paste('SELECT COUNT(*) FROM ', w_tablename, sep='')
  ))
  
  ########################################
  # Cleanup and Return:
  ########################################
  
  # disconnect from database
  dbDisconnect(con)
  
  # unload driver
  dbUnloadDriver(tempdrv)
  
  return(list(numrecords=numrecords))
}
