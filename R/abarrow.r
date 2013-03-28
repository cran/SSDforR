ABarrow <-
function(){
   instruct<-"Click the mouse on the top of the graph where the arrow should begin. Repeat at the point of the graph for where the arrow should end.  "
  print(instruct)
  l<-locator(1)
  l1<-locator(1)
  
  arrows(x0=l$x,x1=l1$x,y0=l$y,y1=l1$y)
  u<-readline("accept line? (y/n) ")
  if (u=="n")
  {replayPlot(ab)}
  if (u=="y")
    ab<-NULL
        ab<<-recordPlot()
  
  
}
