NAPbelow <-
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
  
  plot(iv,cdcl, ylim=c(0,max),lwd=2,type="o",col="red",bty="l", xlab="time", ylab="behavior", main="NAP" )
  
 # writeLines("Find the smallest number of data points you need to remove to eliminate all overlap /ties between phases.")
  #writeLines(" ")
 # yo<-readline("enter largest or smallest baseline data point for reference line  " )
  #yo=min(A)+.1
  #abline(h=yo,col="gray",lwd=3)
  
  ab<-NULL
  
  ab<<-recordPlot()
  
  #rB<-readline("enter number of intervention points remaining " )
 # rA<-readline("enter number of baseline line points to remove  " )
  
  #pA=as.numeric(rA)/length(A)
  
  #pB=as.numeric(rB)/length(B)
  #IRD=(pB-pA)*10
  #IRDP=c(round(IRD,2),"%")
 
  
  nap1<-NAP(A_data = A, B_data = B, SE = "Hanley",improvement="decrease",confidence = 0.95)
  napES<-nap1[[2]]
  napSE<-nap1[[3]]
  napCIL<-nap1[[4]]
  napCIU<-nap1[[5]]
  print(nap1,quote="FALSE")
  writeLines("-------------------------------------------")
  writeLines(".93 or above = very effective" )
  writeLines(".66 to .92 = moderate effectiveness")
  writeLines(" below .66 = not effective")
  writeLines("--------------------------------------------")
  writeLines(" ")
  a<-readline("(s)ave, (a)ppend, or (n)either results? (s/a or n) ")
  
  
  
  ES=data.frame(napES)
  
  if (a=="s")
  {Label<-readline("Enter a behavior variable label ")
  ES<-data.frame(napES,napSE,napCIL,napCIU,Label)
  
  
  write.csv(ES,file = tclvalue(tkgetSaveFile()),row.names=FALSE)
  
  
  } 
  
  if (a=="a")
  { Label<-readline("Enter a behavior variable label ")
  #ES<-cd1
  ES<-data.frame(napES,napSE,napCIL,napCIU,Label)
  
 
  
 
  
  writeLines("*****************open file to append to***************************")
  
  effA<-read.table(file.choose(),header=TRUE,sep=',') 
  out=rbind(effA,ES)
  writeLines(" ")
  writeLines(" ")
  writeLines(" ")
  writeLines("*****************save appended file***************************")  
  
  
  
  write.csv(out,file = tclvalue(tkgetSaveFile()),row.names=FALSE)}
  
}
