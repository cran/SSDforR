plotnum <-
function(nr,nc){
  num = nr*nc
  par( mfrow = c( nr, nc )) 
  mat1<-matrix(1:num,nr,nc)
  
  layout(mat1)
}
