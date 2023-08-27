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
  
  layout(rbind(1,2), heights=c(4,1))
  plot(x,y, ylim=c(min,max),xlim=c(0,numx),lwd=2,type="o",col="red", xlab=ABxlab, ylab=ABylab, main=ABmain,bty='L' )
 
  writeLines("    ")
  writeLines("If you are using Windows 10/11 or RStudio Cloud, cut and paste the link below") 
  writeLines("into your browser to view an article about using ABtext() and ABlines():")
  writeLines("    ")
  writeLines("https://www.dropbox.com/s/ex7ech9atz896yo/If%20you%20are%20using%20Windows%2010%20or%20RStudio%20Cloud.docx?dl=0")
  writeLines("    ")
  writeLines("-------------------------------------------------------------------------------------")
  writeLines("1-You can add lines between phases by using the ABlines() function.")
  writeLines("2-You can add text by using the ABtext() function.")
  writeLines("3-You can add a mean, median, sd line by using the ABstat() function.")
  writeLines("4-You can also add a goal line using the Gline() function.")
  writeLines("-------------------------------------------------------------------------------------")
}
