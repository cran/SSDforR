Effectsize <-
function(behavior,phaseX,v1,v2) {
  mean1 <-tapply(behavior,phaseX,mean,na.rm=T)
  s1 <-tapply(behavior,phaseX, sd, na.rm=T)
 
  tphase<-table(phaseX)
  n1<- tphase[names(tphase)==v1]
  n2<- tphase[names(tphase)==v2]
  
  n1<-n1-1 
  n2<-n2-1
  totaln<-n1+n2
  DIFF<-mean1[names(mean1)==v1]-mean1[names(mean1)==v2]
 
  S<-sqrt(((n1*(s1[names(s1)==v1]^2)+(n2*s1[names(s1)==v2]^2))/totaln))
  CD<-(DIFF/S)
  
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  PA<-phaseX[startA:endA]
         
  tmaxA<-t1[names(t1)==v2]
  startA<-match(v2,phaseX)
  endA<-tmaxA+startA-1
  B<-behavior[startA:endA]
   PB<-phaseX[startA:endA]
  
  
IV<-c(PA,PB)
  DV<-c(A,B)
  
  
 reg1<- summary(lm(DV~IV))
  rvalue2<-reg1$r.squared
  rvalue<-sqrt(reg1$r.squared)
  
  
  DIFF<-mean1[names(mean1)==v2]-mean1[names(mean1)==v1]
  S<-s1[names(s1)==v1]
  es<-(DIFF/S)
  
  l1<-c("small effect size: <.87")
  l2<-c("medium effect size: .87 to 2.67 ")
  l3<-c("large effect size: >2.67")
  writeLines(l1)
  writeLines(l2)
  writeLines(l3)
  writeLines("*************************************************")
  writeLines("********************ES************************")
  
  l4<-c("ES=    ",round(es,3))
  eschange=pnorm(es)-.5
  l5<-c("% change=",round(eschange,4)*100)
  print(c(l4,l5))
  
  
  
  writeLines("**************d-index**************************")
  cd1<-(round(abs(CD),3))
  pcd1<-c("d-index=",cd1)
  dchange=pnorm(CD)-.5
  l6<-c("% change=",round(dchange,4)*100)
  
  print(c(pcd1,l6))
  
  
  writeLines("**************Pearson's r************************")
  print(round(rvalue,3))
  writeLines("**************R-squared**************************")
  print(round(rvalue2,3))
  
}
