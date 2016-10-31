readOracleDB <- function(
  w_host, w_port, w_servicename, 
  w_tablename, w_username, w_password, 
  w_colid, w_wherecond='',
  w_writedir, w_filename, w_fileext='.txt', w_filesep='|',
  w_nperfetch=1e5,
  w_maxnchunks=-1)
  {
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
  # SQL Select Statement:
  ########################################
  
  # string for select statement
  sqlcol <- paste(paste('"', w_colid, '"', sep=''), collapse=',')
  
  # select sqlcol from table
  numres <- dbSendQuery(con, 
                        paste('SELECT', sqlcol, 
                              'FROM', w_tablename, 
                              w_wherecond,
                              sep=' '))
  
  ########################################
  # Define Fetch Function for Loop:
  ########################################
  
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
  
  ########################################
  # Read Entire Database Table:
  ########################################
  
  if (w_maxnchunks == -1) {
    
    # directory to write tables
    setwd(w_writedir)
    
    # get number of records:
    numrow <- dbGetQuery(con, paste('SELECT COUNT(*) FROM', w_tablename, sep=' '))
    
    # fetch w_nperfetch records each call
    nperfetch <- w_nperfetch
    
    if (nperfetch >= numrow) {
      
      cat(w_writedir, 'should contain', 1, 'file.\n')
      
      # write remainder records to file
      tempdt <- fetch(numres)
      write.table(tempdt,
                  paste(w_filename, 0, w_fileext, sep=''), 
                  sep=w_filesep, row.names=FALSE)
      
    } else {
      
      # control variable
      maxk <- as.integer(floor(numrow/nperfetch))
      
      cat(w_writedir, 'should contain at most', maxk+1, 'files.\n')
      
      # write remainder records to file
      tempdt <- fetch(numres, n=(numrow-(maxk*nperfetch)))
      write.table(tempdt,
                  paste(w_filename, 0, w_fileext, sep=''), 
                  sep=w_filesep, row.names=FALSE)
      
      for (k in 1:maxk) {
        tempdt <- safeFetch(k, nperfetch=nperfetch, res=numres)
        if (is.null(tempdt)) {
          break
        } else {
          write.table(tempdt,
                      paste(w_filename, k, w_fileext, sep=''),
                      sep=w_filesep, row.names=FALSE)
        }
      }
      
    }
    
  ########################################
  # Read Subset of Database Table:
  ########################################
    
  } else {
    
    cat(w_writedir, 'should contain at most', w_maxnchunks, 'file(s).\n')
    
    # directory to write tables
    setwd(w_writedir)
    
    # fetch w_nperfetch records each call
    nperfetch <- w_nperfetch
    
    for (k in 0:(w_maxnchunks-1)) {
      tempdt <- safeFetch(k, nperfetch=nperfetch, res=numres)
      if (is.null(tempdt)) {
        break
      } else {
        write.table(tempdt,
                    paste(w_filename, k, w_fileext, sep=''),
                    sep=w_filesep, row.names=FALSE)
      }
    }
    
  }
  
  ########################################
  # Cleanup and Return:
  ########################################
  
  # check all records fetched
  c1 <- dbHasCompleted(numres)
  
  # clear result
  dbClearResult(numres)
  
  # disconnect from database
  dbDisconnect(con)
  
  # unload driver
  dbUnloadDriver(tempdrv)
  
  # return number of records and completion status
  return(list(complete=c1))
  
}
