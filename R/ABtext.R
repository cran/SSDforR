ABtext <-
function(textx){
  instruct<-"Click the mouse where you want the text to begin"
  print(instruct)
  text(locator(1),c(textx),cex=.9)
  u<-readline("accept line? (y/n) ")
  if (u=="n")
  {replayPlot(ab)}
  if (u=="y")
    
    #{assign('ab', recordPlot())}
    ab<-NULL
  ab<<-recordPlot()
  
}
