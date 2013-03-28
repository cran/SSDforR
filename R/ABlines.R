ABlines <-
  function(behavior){
    instruct<-"Click the mouse on the top of the graph where the vertical line should begin. Repeat at the bottom of the graph for where the line should end.  "
    
    l<-locator(1)
    
    
    
    ymin=min(behavior,na.rm = TRUE)
    ymax=max(behavior,na.rm = TRUE)
    ymax=ymax+1
    segments(x0=l$x,y0=ymin,y1=rep(ymin:ymax))
    
   
    
    
    
    u<-readline("accept line? (y/n) ")
    if (u=="n")
    {replayPlot(ab)}
    
      ab<-NULL
    ab<<-recordPlot()
  }
