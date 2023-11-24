ABarrow <-
function(){
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
   writeLines("1-Click the mouse on the top of the graph where the arrow should begin.")
   writeLines("2-Repeat at the point of the graph for where the arrow should end.")
   writeLines("-------------------------------------------------------------------------------------")
  
   l<-locator(1)
  l1<-locator(1)
  
  arrows(x0=l$x,x1=l1$x,y0=l$y,y1=l1$y)
   
   u<-readline("accept line? (y/n) ")
   if (u=="n")
   {replayPlot(targetindex)}
  }
  else {
    writeLines("  ")
    writeLines("ABarrow() command is NOT available for Windows and Posit Cloud operating systems; instead use RSarrow() command.") 
    writeLines("For more information on annotating graphs go to the URL below:")
    writeLines("https://www.dropbox.com/s/ex7ech9atz896yo/If%20you%20are%20using%20Windows%2010%20or%20RStudio%20Cloud.docx?dl=0")
    
  }
  
  
}
