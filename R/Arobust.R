Arobust <-
function(behavior,phaseX,v1){
  DV<-( paste(substitute(behavior)) )
  
  l1<-c("Robust regression for behavior",'"',DV,'"',"in the", v1, "phase")
  writeLines(" ")
  cat(sprintf(l1),"\n")
 
  writeLines("---------------------------------------------- ")
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  
  
  x1=(c(seq(1:tmaxA)))
  
  regA<-rlm(A~x1)
  rA<-residuals(regA)
  yA<-regA$coefficients[1]
  BetaA<-regA$coefficient[2]
  
  
  #graphics.off()
  layout(rbind(1,2), heights=c(6,1))
  plot(x1, A,lwd=2,type="o",col="red", xlab="time", ylab=c(DV,"behavior"), main=v1 )
  abline(c(yA,BetaA),col='Blue',lty="dashed")
  print(summary(regA))
  
  
}
