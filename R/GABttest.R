GABttest <-
function(behavior,phaseX,timeX,v1,v2){
  options (scipen=999)
  options(warn=-1)
  writeLines(" ")
  writeLines("Note: the t-test is unreliable if any phase is autocorrelated or has a trend.")
  writeLines("Use the CDC test if any phase is autocorrelated.")
  writeLines("If no autocorrelation exists in any phase, but a trend exists,") 
  writeLines("use the CDC, regabove, or regbelow test.")
  c<-readline("(c)ontinue or (e)exit and press return (c or e) ")
  if (c=="c"|c=="C") {
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A1<-behavior[startA:endA]
  weekA<-timeX[startA:endA]
  A<-tapply(A1,weekA,mean,na.rm=T)
  meanA=mean(A,na.rm=T)
  # x1=rep(1:tmaxA, each = v1)
  
  
  tmaxB<-t1[names(t1)==v2]
  startB<-match(v2,phaseX)
  endB<-tmaxB+startB-1
  
  B1=(behavior[startB:endB])
 
  weekB<-timeX[startB:endB]
  B<-tapply(B1,weekB,mean,na.rm=T)
  meanB=mean(B,na.rm=T)
  
  t1<-t.test(A,B)
  var1<-var.test(A,B) 
  vartestp<-var.test(A,B)$p.value
  t2<-t.test(A,B,var.equal=TRUE)  
  print(t2)
  print(var1)
  print(t1)
  Means<-c(meanA,meanB) 
  
  if (vartestp>.05) { 
    note1<-c("Note: Variances are equal (F test to compare two variances p-value is greater then 0.05) between phases",v1, "and", v2,".","Use the Two Sample t-test")
    cat(sprintf(note1),"\n")
  }
  
  if (vartestp<=.05) { 
    note2<-c("Note: Variances are unequal (F test to compare two variances p-value is less then or equal to 0.05) between phases ",v1, "and", v2,".","Use the Welch Two Sample t-test")
    cat(sprintf(note2),"\n")
  }
  
  layout(rbind(1,2), heights=c(4,1))
  barplot(Means,ylab="mean",names.arg=c(v1,v2))
  
  }
  options(warn=-0)
  }
