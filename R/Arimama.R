Arimama <-
function(behavior,phaseX,v,m){
  
t1<-table(phaseX)
tmax<-t1[names(t1)==v]
start<-match(v,phaseX)
end<-tmax+start-1
tsx<-behavior[start:end]
e=length(tsx)
x=1:end
x<-ts(x,start=1,end=e,deltat=1)
tsma<-SMA(tsx,n=m)
layout(rbind(1,2), heights=c(6,1))
par(mfrow=c(1,2))
DV<-( paste(substitute(behavior)) )
plot.ts(tsx,xlab="time", ylab=c("moving average  of behavior",DV))
plot.ts(tsma,xlab="time", ylab=c("moving average  of behavior",DV))
}
