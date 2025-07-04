IRDabove <-
function(behavior,phaseX,v1,v2){
  
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  
  maxA=(max(A,na.rm=T))-.1
  
  vx=which.min(A)
  
  
  tmaxB<-t1[names(t1)==v2]
  startB<-match(v2,phaseX)
  endB<-tmaxB+startB-1
  tsxB<-behavior[startB:endB]
  B=(behavior[startB:endB])
  
  
  cdcl<-c(A,NA,B)
  y<-na.omit(cdcl)
  total=length(y)
  iv=(1:total)
  end<-which(is.na(cdcl))
  iv<-insert(iv,NA,end)
  
  
  
  
  #naboveline<-B<maxA
  #nump=sum(naboveline)
  #nx= (length(B)+length(A))-nump
  
  # p=nx/(length(B)+length(A))
  
  maxy=which.max(cdcl)
  
  max<-cdcl[maxy]+1
  numx<-sum(!is.na(cdcl))+3
  par(mfrow=c(3,3)) 
  
  maxy=which.max(behavior)
  
  max<-behavior[maxy]+1
  
  numx<-sum(!is.na(behavior))+3
  
  
  
  
  
  #graphics.off()
  layout(rbind(1,2), heights=c(6,1))
  
  plot(iv,cdcl, ylim=c(0,max),lwd=2,type="o",col="red",bty="l", xlab="time", ylab="behavior", main="IRD" )
  
 # writeLines("Find the smallest number of data points you need to remove to eliminate all overlap /ties between phases.")
  #writeLines(" ")
 # yo<-readline("enter largest or smallest baseline data point for reference line  " )
  yo=max(A)-.1
  abline(h=yo,col="gray",lwd=3)
  
  ab<-NULL
  
  ab<<-recordPlot()
  
  #rB<-readline("enter number of intervention points remaining " )
 # rA<-readline("enter number of baseline line points to remove  " )
  
  #pA=as.numeric(rA)/length(A)
  
  #pB=as.numeric(rB)/length(B)
  #IRD=(pB-pA)*10
  #IRDP=c(round(IRD,2),"%")
  IRD1=IRD(A_data = A, B_data = B)
  IRD2<-IRD1[[2]]*100
  IRDP=c("Est =", round(IRD2,2),"%%")
  #ci<-NAP(A_data = A, B_data = B, SE = "Hanley")
  writeLines(" ")
  DV<-( paste(substitute(behavior)) )
  
  l1<-c("IRD above for behavior",'"',DV,'"')
  writeLines(" ")
  cat(sprintf(l1),"\n")
  writeLines(" ")
  cat(sprintf(IRDP),"\n")
  #print(IRDP,quote="FALSE")

  writeLines("-------------------------------------------")
  writeLines("10th percentile = 36.8" )
  writeLines("25th percentile = 47.9")
  writeLines("50th percentile = 71.8")
  writeLines("75th percentile = 89.8")
  writeLines("90th percentile = 99.9")
}
