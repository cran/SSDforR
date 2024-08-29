RSstat <-
function(behavior,phaseX,v,statX,l){
  targetindex<-dev.cur() 
  targetindex<-recordPlot() 
  
 
  abmedian<-tapply(behavior, phaseX,statX)
  tphase<-table(phaseX)
  mlin<- tphase[names(tphase)==v]
  omedian<- abmedian[names(abmedian)==v]
  stats<-c(statX,round(omedian,3))
 print(stats)
  #l<-locator(1)
 mlin<-l+mlin
  if (statX=="mean") {cl="blue"}   
  if (statX=="median") {cl="darkseagreen"}  
  if (statX=="sd") {cl="darkorange"} 
  if (statX=="mean") {tl=1}   
  if (statX=="median") {tl=2}  
  if (statX=="sd") {tl=4}
  segments(x0=l,x1=mlin,y0=omedian,col=cl,lty=tl,lwd=3)
  u<-readline("accept line? (y/n) ")
  if (u=="n")
  
    
  {replayPlot(targetindex)}
}