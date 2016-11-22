getDBColNames <- function(
  w_host, 
  w_port, 
  w_servicename, 
  w_tablename, 
  w_username, w_password, 
  w_writeflag=FALSE,
  w_filename=''
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
  # Get Column Names:
  ########################################
  
  coldata <- dbGetQuery(con, 
                        paste('SELECT column_name, data_type, data_length FROM all_tab_columns where table_name = ',
                              paste("\'", w_tablename, "\'", sep=''), sep='')
                        )
  
  ########################################
  # Cleanup and Return:
  ########################################
  
  # disconnect from database
  dbDisconnect(con)
  
  # unload driver
  dbUnloadDriver(tempdrv)
  
  if (w_writeflag) {
    write.table(coldata, w_filename, row.names=FALSE, sep=',')
    return(list(complete=TRUE))
  } else {
    return(coldata)
  }
}
