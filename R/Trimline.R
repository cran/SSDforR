Trimline <-
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
  trimmean=mean(A,trim=.1,na.rm=T)
  trm<-c("10 percent Trim Mean=",round(trimmean,3))
  #print(trm)
  writeLines(" ")
  cat(sprintf(trm),"\n") 
  l<-locator(1)
  mlin<-l$x+mlin
  segments(x0=l$x,x1=mlin,y0=trimmean,col="red",lty=2,lwd=3)
  u<-readline("accept line? (y/n) ")
  if (u=="n")
    
    
    
  {replayPlot(targetindex)}
  } 
  else {
    writeLines("  ")
    writeLines("Trimline() command is NOT available for Windows and Posit Cloud operating systems; instead use RSTrimline() command.") 
    writeLines("For more information on annotating graphs go to the URL below:")
    writeLines("https://www.dropbox.com/s/ex7ech9atz896yo/If%20you%20are%20using%20Windows%2010%20or%20RStudio%20Cloud.docx?dl=0")
    
  }
 
}
