sd1bandgraph <-
function(behavior,phaseX,v1,ABxlab,ABylab,ABmain){
t1<-table(phaseX)
tmaxA<-t1[names(t1)==v1]
startA<-match(v1,phaseX)
endA<-tmaxA+startA-1
A<-behavior[startA:endA]

meanA=mean(A,na.rm=T)
sdA=sd(A,na.rm=T)
x1=(c(seq(1:tmaxA)))
SDabove<-meanA+sdA
SDbelow<-meanA-sdA

maxy=which.max(behavior)
max<-behavior[maxy]
#numx<-sum(!is.na(behavior))+1
numx=length(x1)+1
#graphics.off()
layout(rbind(1,2), heights=c(6,1))
plot(A,ylim=c(0,max),xlim=c(0,numx),type="o",xlab=ABxlab, ylab=ABylab, main=ABmain,bty='L')
#legend("bottom",inset=c(-.3,-.73,-.3),xpd = TRUE, c("+1sd","mean","-1sd"), col = c("blue","orange", "red"), lwd = 1,ncol=3,bty ="n")
#legend("bottom", c("+1sd","mean","-1sd"), col = c("blue","orange", "red"), lwd = 1,ncol=3,bty ="n")
abline(h=SDabove,col="blue")
abline(h=SDbelow,col="red")
abline(h=meanA,col="orange")
#par(mar=c(1, 1, 1, 1))
#plot.new()
#legend("bottom",inset=c(-.3,-.1,-.3), c("+1sd","mean","-1sd"), col = c("blue","orange", "red"), lwd = 1,ncol=3,bty ="n")
sd1<-c("  SD =",round(sdA,2))
psdu<-c("+1sd =",round(SDabove,2))
pmean<-c("mean =",round(meanA,2))
psdb<-c("-1SD =",round(SDbelow,2))
tprint=c(sd1,psdu,pmean,psdb)
#print(sd1)
#print(psdu)
#print(pmean)
#print(psdb)
writeLines(" ")
cat(sprintf(tprint),"\n") 

    
 }
