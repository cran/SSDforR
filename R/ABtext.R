ABtext <-
function(textx){
  instruct<-"Click the mouse where you want the text to begin"
  print(instruct)
  text(locator(1),c(textx),cex=.9)
  u<-readline("accept text? (y/n) ")
  if (u=="n")
  {replayPlot(ab)}
  
    
   
    ab<-NULL
  ab<<-recordPlot()
  
}
