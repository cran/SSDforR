Append <-
function(){
  ssdA<-read.table(file.choose(),header=TRUE,sep=',')
  ssdB<-read.table(file.choose(),header=TRUE,sep=',')
  ssdC<-rbind(ssdA,ssdB)
 # {write.csv(ssdC,file = file.choose(new = T),row.names=FALSE)}
{write.csv(ssdC,file = tclvalue(tkgetSaveFile()),row.names=FALSE)}

}
