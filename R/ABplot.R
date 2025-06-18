ABplot <-
function(behavior,phaseX,ABxlab,ABylab, ABmain){
 
  maxy=which.max(behavior)
  max<-behavior[maxy]+1
  numx<-sum(!is.na(behavior))+3
  miny=which.min(behavior)
  min<-behavior[miny]
   y<-na.omit(behavior)
  total=length(y)
  x=(1:total)
  
  end<-which(is.na(phaseX))
  np<-length(end)
  j=1
  while (j <= np){
    e<-end[j]
    
  
    
    y<-insert(y,NA,e)
    x<-insert(x,NA,e)
    j=j+1
  }
  
  
  #\href{http://stackoverflow.com/}{Stack Overflow}
  
  layout(rbind(1,2), heights=c(6,1))
  plot(x,y, ylim=c(min,max),xlim=c(0,numx),lwd=2,type="o",col="red", xlab=ABxlab, ylab=ABylab, main=ABmain,bty='L' )
 
   os <- .Platform$OS.type
  if (grepl("^darwin", R.version$os))
    os <- "osx"
  if (grepl("linux-gnu", R.version$os))
    os <- "linux"
  if (grepl("windows", R.version$os))
    os <- "windows"
 if (os =="osx") {
  writeLines("    ")

  writeLines("-------------------------------------------------------------------------------------")
  writeLines("1-You can add lines between phases by using the ABlines() function.")
  writeLines("2-You can add text by using the ABtext() function.")
  writeLines("3-You can add a mean, median, sd line by using the ABstat() function.")
  writeLines("4-You can also add a goal line using the Gline() function.")
  writeLines("-------------------------------------------------------------------------------------")
 }
  else {
    writeLines("    ")
    
    
    writeLines("    ")
    writeLines("-------------------------------------------------------------------------------------")
    writeLines("1-You can add lines between phases by using the RSlines() function.")
    writeLines("2-You can add text by using the RStext() function.")
    writeLines("3-You can add a mean, median, sd line by using the RSstat() function.")
    writeLines("4-You can also add a goal line using the Gline() function.")
    writeLines("5-You can also add an arrow using the RSarrow() function.")
    writeLines("---------------------------------------------------------------------------------")
    writeLines("For more information on annotating graphs go to the URL below:")
    writeLines("https://www.dropbox.com/s/ex7ech9atz896yo/If%20you%20are%20using%20Windows%2010%20or%20RStudio%20Cloud.docx?dl=0")
 
     }
  
   
 }
