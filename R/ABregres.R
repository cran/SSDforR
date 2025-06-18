ABregres <-
function(behavior,phaseX,v1,v2){
  options (scipen=999)
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  
  meanA=mean(A,na.rm=T)
  x1=(c(seq(1:tmaxA)))
  
  regA<-lm(A~x1)
  rA<-residuals(regA)
  yA<-regA$coefficients[1]
  BetaA<-regA$coefficient[2]
  r2A<-summary(regA)$r.squared
  r2A<-sqrt(r2A)
  writeLines(" ")
  writeLines(" ")
  DV<-( paste(substitute(behavior)) )
  
  l1<-c("Regression for behavior",'"',DV,'"',"in the", v1, "phase")
  
  cat(sprintf(l1),"\n")
  #writeLines("-----------------Baseline------------------")
  print(summary(regA))
  KendalA<-MannKendall(A)
  
  KendalA<-mmky1lag(A)
  ls1<-c("Mann-Kendall Trend Test and Sen's Regression for behavior",DV,"in the", v1, "phase")
  cat(sprintf(ls1),"\n")
  writeLines("--------------------------------------------------------------------------------------")
  #writeLines("-----------------Mann-Kendall Trend Test and Sen's Regression-------------------------")
  print(KendalA)
  writeLines("--------------------------------------------------------------------------------------")
  writeLines("")
  
 # writeLines("----------------- Intervention------------------") 

  tmaxB<-t1[names(t1)==v2]
  startB<-match(v2,phaseX)
  endB<-tmaxB+startB-1
  #tsxB<-behavior[startB:endB]
  B=(behavior[startB:endB])
  x2=(c(seq(1:tmaxB)))
  
  regB<-lm(B~x2)
  rB<-residuals(regB)
  yB<-regB$coefficients[1]
  BetaB<-regB$coefficient[2]
  
  r2B<-summary(regB)$r.squared
  r2B<-sqrt(r2B)
  l2<-c("Regression for behavior",'"',DV,'"',"in the", v2, "phase")
  
  cat(sprintf(l2),"\n")
  print(summary(regB))
  
  
  KendalB<-mmky1lag(B)
  
 # writeLines("-----------------Mann-Kendall Trend Test and Sen's Regression-------------------------")
  ls2<-c("Mann-Kendall Trend Test and Sen's Regression for behavior",DV,"in the", v2, "phase")
  cat(sprintf(ls2),"\n")
  writeLines("--------------------------------------------------------------------------------------")
  print(KendalB)
  writeLines("--------------------------------------------------------------------------------------")
  
  layout(rbind(1,2), heights=c(6,1))
  par(mfrow=c(1,2))
  
  
  plot(x1, A,lwd=2,type="o",col="red", xlab="time", ylab="behavior", bty='L',main=v1 )
  abline(c(yA,BetaA),col='Blue',lty="dashed")
  plot(x2,B,lwd=2,type="o",col="green", xlab="time", ylab="",bty='L', main=v2 )
  abline(c(yB,BetaB),col='Blue',lty="dashed")
 
  
  
 
 
}
