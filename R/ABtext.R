
ABtext <-
function(textx){
  
  targetindex<-dev.cur() 
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
