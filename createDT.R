# source inv-pkgs.R and importFile.R first

createDT <- 
  function(
    w_colid,
    w_newcolid,
    w_nfilegrp=20,
    w_dbpath,
    w_savefile,
    w_writeout=TRUE
  ) {
    
    # import from disk:
    dt <- importFile(
      datapath=w_dbpath,
      filename=list.files(w_dbpath, pattern='.txt'),
      nfilegrp=w_nfilegrp,
      colid=w_colid
    )
    
    # reorder cols:
    setcolorder(dt, w_colid)
    
    # rename cols:
    setnames(dt, colnames(dt), w_newcolid)
    
    
    # output:
    if (w_writeout) {
      save(dt, file=w_savefile)
      cat(w_savefile, 'complete\n')
    } else {
      return(dt)
    }
  }

# for ServerData/Order:
# dt_v3 <- createDT(
#   w_colid=c('ORDER_DATE'),
#   w_newcolid=c('OrderDate'),
#   w_nfilegrp=50,
#   w_dbpath='~/ServerDBData/Order',
#   w_savefile='',
#   w_writeout=FALSE)
