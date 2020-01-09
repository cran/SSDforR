meanNAP <-
  function(es,lab,esmain){
    
    
esmean<-mean(es,na.rm=T)
essd<-sd(es,na.rm=T)
writeLines("-----------mean-------------") 
print(esmean)
writeLines("------------SD--------------")
print(essd)


writeLines("-------------------------------------------")
writeLines(".93 or above = very effective" )
writeLines(".66 to .92 = moderate effectiveness")
writeLines(" below .66 = not effective")
writeLines("--------------------------------------------")
writeLines(" ")


e<-data.frame(es,lab)
e<-e[ order(e$es), ]

#dotchart(es,groups=lab,color="red",cex=.8, xlab="Cohen's D",main=esmain)
dotchart(e$es,labels=e$lab,color="red",cex=.8, xlab="NAP",main=esmain)
    
  }   
