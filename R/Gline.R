Gline <-
  function() {
    targetindex<-dev.cur() 
    targetindex<-recordPlot() 
    
    yo<-readline("Y ordinate for your goal line  " )
abline(h=yo,col="gray",lwd=3)

u<-readline("accept line? (y/n) ")
if (u=="n")
 
{replayPlot(targetindex)}

}