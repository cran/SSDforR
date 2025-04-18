Xmrchart <-
function(behavior,phaseX,v1,bandX,ABxlab,ABylab, ABmain){
  
  max1=which.max(behavior)
  maxy<-behavior[max1]
  min1=which.min(behavior)
  miny<-behavior[min1]
  
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  bmean=mean(A)
  endA=length(A)
  j=2
  diff=seq(1:endA-1)
  for (i in 1:endA) {
    
    j=i-1
    while (j <= endA-1) {
      
      diff[j]<-A[j]-A[i]
      
      j<-j+1
      
    }
  }
  diff<-diff[1:endA-1] 
  rmean<-mean(abs(diff))
  
  btsd<-rmean/1.128
  
  Uband=round(btsd*bandX+bmean)
  Lband=round(bmean-btsd*bandX)
  
 #min=Lband-3
  #max=Uband+3
  f1=Uband >=maxy
  f2=Lband <=miny
  
  if (f1==TRUE)
  {maxy=Uband+1}
  
  
  if (f2==TRUE)
  {miny=Lband-1}
  
 graphics.off()
  y<-na.omit(behavior)
  total=length(y)
  x=(1:total)
  
  end<-which(is.na(phaseX))
  np<-length(end)
  j=1
  while (j <= np){
    e<-end[j]
    
    
    
    y<-insert(y,NA,e)
    x<-insert(x,NA,e)
    j=j+1
  }
  layout(rbind(1,2), heights=c(4,1))
  
  plot(x,y,ylim=c(miny,maxy),type="o",col="red",bty='L',xlab=ABxlab,ylab=ABylab,main=ABmain)
  
  abline(h=bmean,col="green")
  abline(h=Uband,col="blue")
  abline(h=Lband,col="orange")
  
  #par(mar=c(1, 1, 1, 1))
  #plot.new()
  #legend("center", c("behavior","Uband","mean","Lband"), col = c("red","blue", "green","orange"), lwd = 1,ncol=4,bty ="n")
  puband<-c("Uband=",Uband)
  pmean<-c("mean=",round(bmean,2))
  plband<-c("Lband=",Lband)
  #print(puband)
  #print(pmean)
  #print(plband)
  writeLines(" ")
  tprint<-c("Key:",puband,pmean,plband)
  cat(sprintf(tprint),"\n")
 
}
