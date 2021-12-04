RSlines <-
  function(behavior,x){
    
    targetindex<-dev.cur() 
    targetindex<-recordPlot() 
    
    
   # l<-locator(1)
    
    
    
    ymin=min(behavior,na.rm = TRUE)
    ymax=max(behavior,na.rm = TRUE)
    ymax=ymax+1
    segments(x0=x,y0=ymin,y1=rep(ymin:ymax))
    
   
    
    
    u<-readline("accept line? (y/n) ")
    if (u=="n")
      
      
    
    {replayPlot(targetindex)}
    
      ab<-NULL
  ab<<-recordPlot(ab)
    
    
    
  }
