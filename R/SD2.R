SD2 <-
function(behavior,phaseX,v1,ABxlab,ABylab, ABmain){
  
  maxy=which.max(behavior)
  max<-behavior[maxy]
  miny=which.min(behavior)
  min<-behavior[miny]
  
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  meanA=mean(A,na.rm=T)
  sdA=sd(A,na.rm=T)
  SDabove<-meanA+sdA*2
  SDbelow<-meanA-sdA*2
  #min=SDbelow-2
  #max=SDabove+2
  y<-na.omit(behavior)
  total=length(y)
  x=(1:total)
  f1=SDabove >max
  f2=SDbelow <min
  
  if (f1==TRUE)
  {max=SDabove+1}
  
  
  if (f2==TRUE)
  {min=SDbelow-1}
  
  end<-which(is.na(phaseX))
  np<-length(end)
  j=1
  while (j <= np){
    e<-end[j]
    
    
    
    y<-insert(y,NA,e)
    x<-insert(x,NA,e)
    j=j+1
  }
  
  
  layout(rbind(1,2), heights=c(6,1))
  plot(x,y,type="o",ylim=c(min,max),col="red",xlab=ABxlab,ylab=ABylab,main=ABmain,bty="l")
  
  abline(h=meanA,col="green",lwd=3)
  abline(h=SDabove,col="black",lwd=3)
  abline(h=SDbelow,col="black",lwd=3)
  
  
  sdp<-c("SD =",round(sdA,2))
  psdu<-c("+2SD =",round(SDabove,2))
  pmean<-c("mean =",round(meanA,2))
  psdb<-c("-2SD =",round(SDbelow,2))
  
  
  tprint=c(sdp,psdu,pmean,psdb)
  #print(tprint)
  writeLines(" ")
  cat(sprintf(tprint),"\n") 
}
