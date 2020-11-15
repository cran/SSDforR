plotnum <-
function(nr,nc){
  num = nr*nc
  par( mfrow = c( nr, nc )) 
  mat1<-matrix(1:num,nr,nc)
  #layout(rbind(1,2), heights=c(1,1))
  
  layout(mat1)
}
