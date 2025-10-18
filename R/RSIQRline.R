RSIQRline <-
function(behavior,phaseX,v,l){
  
    targetindex<-dev.cur() 
    targetindex<-recordPlot() 
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
  q=quantile(A,na.rm=T)
  
  
  p75<-q[4]
  p25<-q[2]
  omedianu<-p75
  omedianb<-p25
  iqr1<-omedianu-omedianb
  
  p2575<-c("p75=",omedianu,"p25=",iqr1,"IQR=",iqr1)
  #print(p2575)
  p2575<-as.character(p2575)
  writeLines(" ")
  cat(sprintf(p2575),"\n") 
  writeLines(" ")
  
  #l<-locator(1)
  mlin<-l+mlin
  #segments(x0=l,x1=mlin,y0=iqr1,col="black",lty=2,lwd=3)
  segments(x0=l,x1=mlin,y0=iqr1,col="black",lwd=3)
  #l<-locator(1)
 
 u<-readline("accept line? (y/n) ")
 if (u=="n")
   
   
 {replayPlot(targetindex)}
  } 
  


