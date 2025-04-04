ABstat <-
function(behavior,phaseX,v,statX){
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
  abmedian<-tapply(behavior, phaseX,statX)
  tphase<-table(phaseX)
  mlin<- tphase[names(tphase)==v]
  omedian<- abmedian[names(abmedian)==v]
  stats<-c(statX,"=",round(omedian,3))
 #print(stats)
  writeLines(" ")
  cat(sprintf(stats),"\n")
  l<-locator(1)
  mlin<-l$x+mlin
  if (statX=="mean") {cl="blue"}   
  if (statX=="median") {cl="darkseagreen"}  
  if (statX=="sd") {cl="darkorange"} 
  if (statX=="mean") {tl=1}   
  if (statX=="median") {tl=2}  
  if (statX=="sd") {tl=4}
  segments(x0=l$x,x1=mlin,y0=omedian,col=cl,lty=tl,lwd=3)
  u<-readline("accept line? (y/n) ")
  if (u=="n")
  
    
  {replayPlot(targetindex)}
  }
  else {
    writeLines("  ")
    writeLines("ABstat() command is NOT available for Windows and Posit Cloud operating systems; instead use RSstat() command.") 
    writeLines("For more information on annotating graphs go to the URL below:")
    writeLines("https://www.dropbox.com/s/ex7ech9atz896yo/If%20you%20are%20using%20Windows%2010%20or%20RStudio%20Cloud.docx?dl=0")
    
  }
  
}
