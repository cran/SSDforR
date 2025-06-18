meanNAP <-
  function(es,lab,esmain){
    
    
esmean<-mean(es,na.rm=T)
essd<-sd(es,na.rm=T)
esmean<-as.character(round(esmean,3))
essd<-as.character(round(essd,3))
writeLines("")
writeLines("mean")
#print(esmean)
cat(sprintf(esmean),"\n")
writeLines("------------")
writeLines("SD")
#print(essd)
cat(sprintf(essd),"\n")
writeLines("")
writeLines("-------------------------------------------")
writeLines(".93 or above = very effective" )
writeLines(".66 to .92 = moderate effectiveness")
writeLines(" below .66 = not effective")
writeLines("--------------------------------------------")
writeLines(" ")


e<-data.frame(es,lab)
e<-e[ order(e$es), ]
layout(rbind(1,2), heights=c(6,1))

#dotchart(es,groups=lab,color="red",cex=.8, xlab="Cohen's D",main=esmain)
dotchart(e$es,labels=e$lab,color="red",cex=.8, xlab="NAP",main=esmain)
    
  }   
