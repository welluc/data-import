# source inv-pkgs.R and importFile.R first

createDT <- 
  function(
    w_colid,
    w_newcolid,
    w_nfilegrp=20,
    w_filepath,
    w_savefile,
    w_writeout=TRUE
  ) {
    
    # import from disk:
    dt <- importFile(
      filename=w_filepath,
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

# Example:
# dt_v4 <- createDT(
#   w_colid=c('Valuation Date', 'Branch Number', 'Product Number', 'Primary UPC'),
#   w_newcolid=c('ValDate', 'BranchNumber', 'EclipseID', 'PrimaryUPC'),
#   w_nfilegrp=20,
#   w_filepath=list.files('C:/Users/datausr/Desktop/ServerDBData/Inventory', pattern='.txt', full.names=TRUE),
#   w_savefile='',
#   w_writeout=FALSE)
