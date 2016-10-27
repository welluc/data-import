readOracleDB <- function(
  w_host, w_port, w_servicename, 
  w_tablename, w_username, w_password, 
  w_colid, w_writedir, w_filename, w_fileext='.txt', w_filesep='|',
  w_nperfetch=1e5)
  {
  require(ROracle)
  
  drv <- dbDriver("Oracle")
  
  connectstring <- paste(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", w_host, ")(PORT=", w_port, "))",
    "(CONNECT_DATA=(SERVICE_NAME=", w_servicename, ")))", sep = "")
  
  # connect to database
  con <- dbConnect(drv, username = w_username, password = w_password,
                   dbname = connectstring)
  
  # string for select statement
  sqlcol <- paste(paste('"', w_colid, '"', sep=''), collapse=',')
  
  # directory to write tables
  setwd(w_writedir)
  
  # function to loop over
  safeFetch <- function(k, nperfetch=1000, res) {
    # k is looping variable. printed to console to track progress.
    # nperfetch is number of records to fetch
    # res is the result from dbSendQuery()
    
    # if not all records fetched
    if (!dbHasCompleted(res)) {
      print(k)
      return(fetch(res, n=nperfetch))
    }
    # else do nothing
  }
  
  # get number of records:
  numrow <- dbGetQuery(con, paste('SELECT COUNT(*) FROM', w_tablename, sep=' '))
  
  # select sqlcol from table
  numres <- dbSendQuery(con, 
                        paste('SELECT', sqlcol, 'FROM', w_tablename, sep=' '),
                        prefetch=TRUE, bulk_read=1000)
  
  # fetch w_nperfetch records each call
  nperfetch <- w_nperfetch
  
  # control variable
  maxk <- as.integer(floor(numrow/nperfetch))
  
  cat(w_writedir, 'should contain', maxk+1, 'files.\n')
  
  # write remainder records to file
  tempdt <- fetch(numres, n=(numrow-(maxk*nperfetch)))
  write.table(tempdt,
              paste(w_filename, 0, w_fileext, sep=''), 
              sep=w_filesep, row.names=FALSE)
  
  if (maxk > 0) {
    # write nperfetch records to disk each time
    for (k in 1:maxk) {
      tempdt <- safeFetch(k, nperfetch=nperfetch, res=numres)
      write.table(tempdt, 
                  paste(w_filename, k, w_fileext, sep=''), 
                  sep=w_filesep, row.names=FALSE)
    }
  }
  
  # check all records fetched
  c1 <- dbHasCompleted(numres)
  
  # clear result
  dbClearResult(numres)
  
  # disconnect from database
  dbDisconnect(con)
  
  # unload driver
  dbUnloadDriver(drv)
  
  # return number of records and completion status
  return(list(numrecords=numrow, complete=c1))
  
}
