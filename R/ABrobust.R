ABrobust <-
function(behavior,phaseX,v1,v2){
  
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  
  meanA=mean(A,na.rm=T)
  x1=(c(seq(1:tmaxA)))
  
  regA<-rlm(A~x1)
  rA<-residuals(regA)
  yA<-regA$coefficients[1]
  BetaA<-regA$coefficient[2]
  
  
  
  tmaxB<-t1[names(t1)==v2]
  startB<-match(v2,phaseX)
  endB<-tmaxB+startB-1
  #tsxB<-behavior[startB:endB]
  B=(behavior[startB:endB])
  x2=(c(seq(1:tmaxB)))
  
  regB<-rlm(B~x2)
  rB<-residuals(regB)
  yB<-regB$coefficients[1]
  BetaB<-regB$coefficient[2]
  
  
  
  layout(rbind(1,2), heights=c(6,1))
  par(mfrow=c(1,2))
  
  DV<-( paste(substitute(behavior)) )
  plot(x1, A,lwd=2,type="o",col="red", xlab="time", ylab=c(DV,"behavior"), bty='L',main=v1 )
  abline(c(yA,BetaA),col='Blue',lty="dashed")
  plot(x2,B,lwd=2,type="o",col="green", xlab="time", ylab=c(DV,"behavior"),bty='L', main=v2 )
  abline(c(yB,BetaB),col='Blue',lty="dashed")
  writeLines(" ")
  
  
  l1<-c("Robust regression for behavior",'"',DV,'"',"in the", v1, "phase")
  
  writeLines("------------------------------------------------------------")
  cat(sprintf(l1),"\n")
  
  print(summary(regA))
  
  DV<-( paste(substitute(behavior)) )
  
  
  writeLines("------------------------------------------------------------")
 
  l2<-c("Robust regression for behavior",'"',DV,'"',"in the", v2, "phase")
  
  cat(sprintf(l2),"\n")
  print(summary(regB))
 
}
