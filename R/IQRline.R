IQRline <-
function(behavior,phaseX,v){
  os <- .Platform$OS.type
  if (grepl("^darwin", R.version$os))
    os <- "osx"
  if (grepl("linux-gnu", R.version$os))
    os <- "linux"
  if (grepl("windows", R.version$os))
    os <- "windows"
  if (os =="osx") { 
    targetindex<-dev.cur() 
    targetindex<-recordPlot() 
  targetindex<-dev.cur() 
  targetindex<-recordPlot() 
  
  writeLines("-------------------------------------------------------------------------------------")
  writeLines("Click the mouse in the beginning of the phase you want the line in")
  writeLines("-------------------------------------------------------------------------------------")
  abmedian<-tapply(behavior, phaseX,sd)
  meansd<-tapply(behavior, phaseX,mean)
  tphase<-table(phaseX)
  mlin<- tphase[names(tphase)==v]
  omedian<- abmedian[names(abmedian)==v]
  meansd1<-meansd[names(meansd)==v]
  
  t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v]
  startA<-match(v,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  q=quantile(A,na.rm=T)
  
  
  p75<-q[4]
  p25<-q[2]
  omedianu<-p75
  omedianb<-p25
  iqr1<-omedianu-omedianb
  
  p2575<-c("p75=",omedianu,"p25=",omedianb,"IQR=",iqr1)
  #print(p2575)
  p2575<-as.character(p2575)
  writeLines(" ")
  cat(sprintf(p2575),"\n") 
  writeLines(" ")
  l<-locator(1)
  mlin<-l$x+mlin+1
  segments(x0=l$x,x1=mlin,y0=iqr1,lwd=3,col="black")
  #l<-locator(1)
 
 u<-readline("accept line? (y/n) ")
 if (u=="n")
   
   
 {replayPlot(targetindex)}
  } 
  else {
    writeLines("  ")
    writeLines("Trimline() command is NOT available for Windows and Posit Cloud operating systems; instead use RSIQRline() command.") 
    writeLines("For more information on annotating graphs go to the URL below:")
    writeLines("https://www.dropbox.com/s/ex7ech9atz896yo/If%20you%20are%20using%20Windows%2010%20or%20RStudio%20Cloud.docx?dl=0")
    
  }
}

