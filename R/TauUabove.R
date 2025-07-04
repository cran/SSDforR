TauUabove <-
function(behavior,phaseX,v1,v2){
 
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  
  minA=(min(A,na.rm=T))-.1
  vx=which.min(A)
  
  
  tmaxB<-t1[names(t1)==v2]
  startB<-match(v2,phaseX)
  endB<-tmaxB+startB-1
  #tsxB<-behavior[startB:endB]
  B=(behavior[startB:endB])
  
  
  cdcl<-c(A,NA,B)
  y<-na.omit(cdcl)
  total=length(y)
  iv=(1:total)
  end<-which(is.na(cdcl))
  iv<-insert(iv,NA,end)
  maxy=which.max(cdcl)
  
  max<-cdcl[maxy]+1
  numx<-sum(!is.na(cdcl))+3
  par(mfrow=c(3,3)) 
  
  maxy=which.max(behavior)
  
  max<-behavior[maxy]+1
  
  numx<-sum(!is.na(behavior))+3
  
  
  DV<-( paste(substitute(behavior)) )
  
  l1<-c("Tau-U above for behavior",'"',DV,'"')
  writeLines(" ")
  cat(sprintf(l1),"\n")
  writeLines(" ")
  
  tau<-Tau_U(A_data = A, B_data = B,improvement="increase")
  print(tau)
  writeLines(" ")
  writeLines("*********************** baseline-corrected Tau index (Tarlow 2017)")
  writeLines(" ")
  taubc<-Tau_BC(A_data = A, B_data = B,improvement="increase",pretest_trend = .05,Kendall=TRUE)
  print(taubc)
}
