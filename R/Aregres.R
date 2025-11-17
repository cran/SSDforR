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
  
  DV<-( paste(substitute(behavior)) )
 
   layout(rbind(1,2), heights=c(6,1))
  
  plot(x1, A,lwd=2,type="o",col="red", xlab="time", ylab=c(DV,"behavior"), bty='L',main=c(v1,"Phase" ))
  abline(c(yA,BetaA),col="blue",lty="dashed")
  writeLines(" ")
  par(mar=c(.5, .5, .5, .5))
  plot.new()
  legend("center", c("behavior","regression line"), col = c("red","blue"),lty =c ("solid","dashed"), lwd = 1,ncol=2,bty ="o",cex=.8)  
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  layout(rbind(1,2), heights=c(6,1))

   l1<-c("Regression for behavior",'"',DV,'"',"in the", v1, "phase")
  
  cat(sprintf(l1),"\n")
  
  
  
   print(summary(regA))
  
  
  
  
  KendalA<-mmky1lag(A)
  
  ls1<-c("Mann-Kendall Trend Test and Sen's Regression for behavior",DV,"in the", v1, "phase")
  cat(sprintf(ls1),"\n")
  writeLines("--------------------------------------------------------------------------------------")
  #writeLines("-----------------Mann-Kendall Trend Test and Sen's Regression-------------------------")
print(KendalA)
  writeLines("--------------------------------------------------------------------------------------")
  
}
