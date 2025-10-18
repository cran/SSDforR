meanabove <-
function (behavior,phaseX,v1,v2) {
  options (scipen=999)
  writeLines(" ")
  DV<-( paste(substitute(behavior)) )
  
  l1<-c("Chi-square mean above for behavior",'"',DV,'"')
  cat(sprintf(l1),"\n")
  writeLines(" ")
  medianab<-tapply(behavior,phaseX,mean)
  medianx=medianab[names(medianab)==v1]
  dzone<-behavior > medianx
  tm<-table(dzone,phaseX) 
  ctbl<-cbind(tm[,v1],tm[,v2])
  num<-ctbl[4]
  colnames (ctbl)<-c("Baselene", "Intervention")
  pre<-(prop.table(ctbl,2)*100)
  numpre<-pre[4]
  nl<-c("There are", as.character(num),"value(s)","(",as.character(round(numpre,2)), "percent) above the mean line in the intervention.")
  #nl<-c("There are", as.character(num),"value(s) above the mean line in the intervention.")
  writeLines(" ")
  writeLines("Note: Intervention TRUE values above the mean line are desired.")
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
  plot(behavior,col="red",type="o",xlab="time",ylab=c(DV,"behavior"),main="Mean Above")
  abline(h=medianx,col="green")
  par(mar=c(1, 1, 1, 1))
  plot.new()
  
  legend("center", c("behavior","mean"), col = c("red","green"), lwd = 1,ncol=2,bty ="o",cex=.8)  
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  layout(rbind(1,2), heights=c(6,1))
}
