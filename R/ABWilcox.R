ABWilcox <-
function(behavior,phaseX, v1,v2){
  options(warn=-1)
  options(scipen=9999)
  writeLines(" ")
  writeLines("Note: the Wilcoxon test is unreliable if any phase is autocorrelated or has a trend.")
  writeLines("Use the CDC test if any phase is autocorrelated.")
  writeLines("If no autocorrelation exists in any phase, but a trend exists,") 
  writeLines("use the CDC, regabove, or regbelow test.")
  c<-readline("(c)ontinue or (e)xit and press return (c or e) ")
  if (c=="c"|c=="C") {
    writeLines(" ")
    writeLines(" ")
    DV<-( paste(substitute(behavior)) )
    
    l1<-c("Wilcoxon test for behavior",'"',DV,'"')
    cat(sprintf(l1),"\n")
    writeLines(" ")
   t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  meanA=mean(A,na.rm=T)
  # x1=rep(1:tmaxA, each = v1)
  
  
  tmaxB<-t1[names(t1)==v2]
  startB<-match(v2,phaseX)
  endB<-tmaxB+startB-1
  
  B=(behavior[startB:endB])
  y=c(A,B)
  xa=rep(1:1,length(A))
  xb=rep(2:2,length(B))
  x=c(xa,xb)       
  meanB=mean(B,na.rm=T)
  u<-wilcox.test(y~x,correct=FALSE,exact=F)
  #print(y)
  #print(x)
  writeLines(" ")
  m<-c(v1,"-means-",v2, v1,"-medians-",v2)
  cat(sprintf(m),"\n")
  
  meanA<-as.character(round(meanA,digits=2))
  meanB<-as.character(round(meanB,digits = 2))
  Means<-c(meanA,meanB) 
 
  medianA=median(A,na.rm=T)
  medianB=median(B,na.rm=T)
  medianA<-as.character(round(medianA,digits=2))
  medianB<-as.character(round(medianB,digits = 2))
  Median<-c("    ",medianA,"  ",medianB) 
   
  cat(sprintf(c(Means,Median)),"\n")
  print(u)
 
  layout(rbind(1,2), heights=c(6,1))
  boxplot(behavior~phaseX,xlab="Phase")
  }  
  options(warn=-0)}
