ABlines <-
  function(behavior){
    
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
