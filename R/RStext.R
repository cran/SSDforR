
RStext <-
function(textx,x,y){
  
  targetindex<-dev.cur() 
  targetindex<-recordPlot() 
  
  
  text(x,y,c(textx),cex=.9)
  u<-readline("accept text? (y/n) ")
  if (u=="n")

  {replayPlot(targetindex)}
  
    
   
    ab<-NULL
  ab<<-recordPlot()
  
}
