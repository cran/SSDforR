ABbinomial <-
function(phaseX,v1,v2,successA,successB){
 options (scipen=999)
  DV<-( paste(substitute(phaseX)) )
  DV<-substring(DV, 2)
  
  #l1<-c("Proportion Frequency/Binomial Test comparing phases", v1,"and",v2,":")
  l1<-c("Proportion Frequency/Binomial Test comparing phases", v1,"and",v2,"for",'"',DV,'"',"behavior")
   writeLines(" ")
  cat(sprintf(l1),"\n")
  
   t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  tmaxB<-t1[names(t1)==v2]
  psucessA=successA/tmaxA
  b=binom.test(x=successB, n=tmaxB, p=psucessA)
  writeLines(" ")
  print(b)
 
}
