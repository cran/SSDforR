Getcsv <-
function(){
  m<-match("ssd",search())
  O<- sum(m,na.rm=T)
  if (O >0) {detach(ssd)}
  
  ssd<-NULL
  
  ssd<<-read.table(file.choose(),header=TRUE,sep=',')
  
  writeLines("--------------------------------------------------------------------------------------")
  writeLines("PLEASE FOLLOW THESE INSTRUCTIONS ") 
  writeLines("1-Type attach(ssd) in the console and press <RETURN> to begin working with the file")
  writeLines("2-Type listnames() in the console and press <RETURN> to list you variable names")
  writeLines("3-Before opening another file type detach(ssd) and press <RETURN>")
  writeLines("--------------------------------------------------------------------------------------")
  writeLines(" ")
  writeLines("Go to www.ssdanalysis.com for more information")
  
 
}
