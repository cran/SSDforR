trendtest <-
function(behavior,phaseX,v1){
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  
  
  x1=(c(seq(1:tmaxA)))
  
 
  

  layout(rbind(1,2), heights=c(4,1))

  
  
  KendalA<-mmky1lag(A)
  writeLines(" ")
  
  DV<-( paste(substitute(behavior)) )
  
  l1<-c("Mann-Kendall Trend Test and Sen's Regression for behavior",'"',DV,'"',"in the", v1, "phase")
  cat(sprintf(l1),"\n")
  writeLines("-------------------------------------------------------------------------------------")
  #writeLines("-----------------Mann-Kendall Trend Test and Sen's Regression-------------------------")
  print(KendalA)
  writeLines("--------------------------------------------------------------------------------------")
  
 
  
}
