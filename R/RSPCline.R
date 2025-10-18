RSPCline<-function(x,ymin,ymax){
  
  
  lines(c(x, x), c(ymin, ymax))
 
  
 # segments(x0=x,y0=ymin,y1=rep(ymin:ymax))
 
  u<-readline("accept line? (y/n) ")
  if (u=="n")
  {replayPlot(ab)}
  
  ab<-NULL
  ab<<-recordPlot()

}
