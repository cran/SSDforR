Aregres <-
function(behavior,phaseX,v1){
  options (scipen=999)
   t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  
  
  x1=(c(seq(1:tmaxA)))
  
  regA<-lm(A~x1)
  rA<-residuals(regA)
  yA<-regA$coefficients[1]
  BetaA<-regA$coefficient[2]
  

  layout(rbind(1,2), heights=c(6,1))
  #par(mar = rep(2, 4))
  plot(x1, A,lwd=2,type="o",col="red", xlab="time", ylab="behavior", bty='L',main=v1 )
  abline(c(yA,BetaA),col='Blue',lty="dashed")
  writeLines(" ")
  DV<-( paste(substitute(behavior)) )

   l1<-c("Regression for behavior",'"',DV,'"',"in the", v1, "phase")
  
  cat(sprintf(l1),"\n")
  
  
  
   print(summary(regA))
  
  
  KendalA<-MannKendall(A)
  
  KendalA<-mmky1lag(A)
  
  ls1<-c("Mann-Kendall Trend Test and Sen's Regression for behavior",DV,"in the", v1, "phase")
  cat(sprintf(ls1),"\n")
  writeLines("--------------------------------------------------------------------------------------")
  #writeLines("-----------------Mann-Kendall Trend Test and Sen's Regression-------------------------")
print(KendalA)
  writeLines("--------------------------------------------------------------------------------------")
  
}
