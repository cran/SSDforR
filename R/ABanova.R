ABanova <-
function(behavior,phaseX){
  options(warn=-1)
   options(scipen=999)
   writeLines(" ")
   writeLines("Note: the ANOVA test is unreliable if any phase is autocorrelated or has a trend.")
   writeLines("Use the CDC test if any phase is autocorrelated.")
   writeLines("If no autocorrelation exists in any phase, but a trend exists,") 
   writeLines("use the CDC, regabove, or regbelow test.")
   c<-readline("(c)ontinue or (e)xit and press return (c or e) ")
   if (c=="c"|c=="C") {
   writeLines(" ")
     DV<-( paste(substitute(behavior)) )
     
     l1<-c("One-way ANOVA for behavior",'"',DV,'"')
     cat(sprintf(l1),"\n")
     writeLines(" ")
  behavior <-na.omit(behavior)
  phase<-na.omit(phaseX)
 phase <- as.character(phase[phase!=""])
  meanA<-tapply(behavior,phase,mean,rm.na=T)
  #aovx<-aov(behavior~as.factor(phaseX))
  aovx<-aov(behavior~phase)
  print(summary(aovx))
  writeLines(" ")
  writeLines("------------means------------------")
  print(round(meanA,digits=3))
  writeLines("-----------------------------------")
  writeLines(" ")
  tukeyx<-TukeyHSD(aovx)
  #graphics.off()
  layout(rbind(1,2), heights=c(6,1))
  plot(tukeyx)
  print(tukeyx)
  bartest<-bartlett.test(behavior~phase)
  print(bartest)
  bnote<-c("A p-value above 0.05 indicates homogeneity of variances." )
  cat(sprintf(bnote),"\n")
 
}

   options(warn=-0)}