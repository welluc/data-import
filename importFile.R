importFile <- function(filename, nfilegrp, colid) {
  
  rbindLoop <- function(dt, rbl_filename=filename, rbl_colname=colid) {
    
    if (length(rbl_filename) > 0 & nrow(dt) > 0) {
      j <- 1
      
      # only execute loop if still have files to read
      MAXITER <- length(rbl_filename)
      while (j <= MAXITER) {
        
        tempdt <- fread(
          input=rbl_filename[j],
          header=TRUE,
          verbose=FALSE,
          showProgress=FALSE,
          select=rbl_colname
        )
        
        dt <- rbind(dt, tempdt)
        
        j <- j + 1
      }
      
      return(dt)
      
    } else {
      return(NA)
    }
  }
  
  fileindex <- as.integer(seq(1, length(filename), length.out=nfilegrp))
  MAXFILEINDEX <- length(fileindex)
  
  foo <- function(b, foofileindex=fileindex, foofilename=filename, fooinvcolid=colid) {
    dt <- fread(
      input=foofilename[foofileindex[b]],
      header=TRUE,
      verbose=FALSE,
      showProgress=FALSE,
      select=fooinvcolid
    )
    
    if (b < (MAXFILEINDEX - 1)) {
      dt <- rbindLoop(dt, rbl_filename=foofilename[(foofileindex[b]+1):(foofileindex[b+1]-1)])
      return(dt)
    } else {
      dt <- rbindLoop(dt, rbl_filename=foofilename[(foofileindex[b]+1):(foofileindex[MAXFILEINDEX])])
      return(dt)
    }
    
  }
  
  
  cl <- makeCluster(4)
  registerDoParallel(cl)
  dt <- foreach(b=1:(MAXFILEINDEX-1), .packages='data.table') %dopar% {
    foo(b)
  }
  stopCluster(cl)
  
  dt <- rbindlist(dt)
  return(dt)
}
