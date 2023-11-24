
ABtext <-
function(textx){
  os <- .Platform$OS.type
  if (grepl("^darwin", R.version$os))
    os <- "osx"
  if (grepl("linux-gnu", R.version$os))
    os <- "linux"
  if (grepl("windows", R.version$os))
    os <- "windows"
  if (os =="osx") {
  #targetindex<-dev.cur()
  targetindex<-recordPlot() 
  
  text<-NULL
  writeLines("------------------------------------------------------------------------")
  writeLines("Click the mouse where you want the text to begin.")
  writeLines("------------------------------------------------------------------------")
  
  text(locator(1),c(textx),cex=.9) 
  u<-readline("accept text? (y/n) ")
  if (u=="n")

  {replayPlot(targetindex)}
  
    
   
    #ab<-NULL
  #ab<<-recordPlot()
  }
  else {
    writeLines("  ")
    writeLines("ABtext() command is NOT available for Windows and Posit Cloud operating systems; instead use RStext() command.") 
    writeLines("For more information on annotating graphs go to the URL below:")
    writeLines("https://www.dropbox.com/s/ex7ech9atz896yo/If%20you%20are%20using%20Windows%2010%20or%20RStudio%20Cloud.docx?dl=0")
    
  }

  }
