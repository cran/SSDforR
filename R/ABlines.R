ABlines <-
  function(behavior){
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
    if (grepl("windows", R.version$os))
      os <- "windows"
    if (os =="osx") {
    targetindex<-dev.cur() 
    targetindex<-recordPlot() 
    
    writeLines("-------------------------------------------------------------------------------------")
    writeLines("Click the mouse in the gap between the phases you want the line in.")
    writeLines("-------------------------------------------------------------------------------------")
    l<-locator(1) 
    
    
    
    ymin=min(behavior,na.rm = TRUE)
    ymax=max(behavior,na.rm = TRUE)
    ymax=ymax+1
    segments(x0=l$x,y0=ymin,y1=rep(ymin:ymax))
    
   
    
    
    u<-readline("accept line? (y/n) ")
    if (u=="n")
      
      
    
    {replayPlot(targetindex)}
    
      #ab<-NULL
  #ab<<-recordPlot(ab)
    }
    else {
      writeLines("  ")
      writeLines("ABlines() command is NOT available for Windows and Posit Cloud operating systems; instead use RSlines() command.") 
      writeLines("For more information on annotating graphs go to the URL below:")
      writeLines("https://www.dropbox.com/s/ex7ech9atz896yo/If%20you%20are%20using%20Windows%2010%20or%20RStudio%20Cloud.docx?dl=0")
      
    }
    
    
  }
