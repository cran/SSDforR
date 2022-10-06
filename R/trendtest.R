trendtest <-
function(behavior,phaseX,v1){
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  
  
  x1=(c(seq(1:tmaxA)))
  
  regA<-MannKendall(A)
  

  layout(rbind(1,2), heights=c(4,1))
  
  writeLines("-----------------Mann-Kendall trend test------------------")
  print(regA)
  writeLines("----------------------------------------------------------")
  
}
