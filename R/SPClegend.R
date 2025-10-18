SPClegend <-
function(){
  
  par(mar=c(1, 1, .2, 1))
  plot.new()
  legend("center", c("Behavior","Uband","Mean","Lband"), col = c("red","blue", "green","orange"), lwd = 1,ncol=4,bty ="o",cex=.8)
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  layout(rbind(1,2), heights=c(6,1))
  }
