RSTrimline <-
function(behavior,phaseX,v,l){
  
  targetindex<-dev.cur() 
  targetindex<-recordPlot() 
  
  
  abmedian<-tapply(behavior, phaseX,sd)
  meansd<-tapply(behavior, phaseX,mean)
  tphase<-table(phaseX)
  mlin<- tphase[names(tphase)==v]
  omedian<- abmedian[names(abmedian)==v]
  meansd1<-meansd[names(meansd)==v]
  
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v]
  startA<-match(v,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  trimmean=mean(A,trim=.1,na.rm=T)
  trm<-c("10 percent Trim Mean=",round(trimmean,3))
  #print(trm)
  writeLines(" ")
  cat(sprintf(trm),"\n") 
  writeLines(" ")
  #l<-locator(1)
  mlin<-l+mlin
  segments(x0=l,x1=mlin,y0=trimmean,col="red",lty=2,lwd=3)
  u<-readline("accept line? (y/n) ")
  if (u=="n")
    
    
    
  {replayPlot(targetindex)}
 
 
}
