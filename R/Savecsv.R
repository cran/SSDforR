Savecsv<-
  function(){
    
  
    outdat <- get("ssd", envir  = environment())
    if (exists('ssd')) write.csv(outdat,file = file.choose(new = TRUE),row.names=FALSE)
  }              