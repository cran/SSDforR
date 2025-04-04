medbelow <-
function (behavior,phaseX,v1,v2) {
  medianab<-tapply(behavior,phaseX,median)
  medianx= medianx=medianab[names(medianab)==v1]
  dzone<-behavior < medianx
  tm<-table(dzone,phaseX) 
  
  ctbl<-cbind(tm[,v1],tm[,v2])
  ctbl<-as.table(ctbl)
  
  
  num<-ctbl[4]
  pre<-(prop.table(ctbl,2)*100)
  numpre<-pre[4]
  nl<-c("There are", as.character(num),"value(s)","(",as.character(round(numpre,2)), "percent) below the median line in the intervention.")
  
  colnames (ctbl)<-c("Baselene", "Intervention")
  
  writeLines(" ")
  writeLines("Note: Intervention TRUE values below the median line are desired.")
  cat(sprintf(nl),"\n")
  
  writeLines(" ")
  writeLines("Frequencies")
  print(ctbl)
  writeLines(" ")
  writeLines("Row Percent")
  print(prop.table(ctbl,1)*100)
  writeLines(" ")
  writeLines("Column Percent")
  print(prop.table(ctbl,2)*100)
  writeLines(" ")
  c1<-chisq.test(ctbl,simulate.p.value = TRUE)
  f1<-fisher.test(ctbl,alternative = "two.sided",conf.int = FALSE)
  print(c1)
  print(f1)
  #graphics.off()
  layout(rbind(1,2), heights=c(6,1))
  plot(behavior,col="red",type="o")
  abline(h=medianx,col="green")
  par(mar=c(1, 1, 1, 1))
  plot.new()
  legend("center", c("behavior","median"), col = c("red","green"), lwd = 1,ncol=2,bty ="n")  
}
