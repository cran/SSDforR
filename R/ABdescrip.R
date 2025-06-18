ABdescrip <-
  function(behavior,PhaseX){
   
    options(warn=-1)
    DV<-( paste(substitute(behavior)) )
    
    l1<-c("Descriptive statistics for behavior",'"',DV,'"')
    writeLines(" ")
    cat(sprintf(l1),"\n")
    
writeLines(" ")
    writeLines("-----------Number of Observations------------")
  #remove blanks 
     Ns <- as.character(PhaseX[PhaseX!=""])
    
    Freq<-table (Ns)
    
    Freq<-as.data.frame.table(Freq)
    
    
    len=length(Freq)

    if (len > 1 )  {
   colnames(Freq)<- c("Phase","Freq") 
  
  }  else 
   {  colnames(Freq)<- c("Freq") 
   } 
    Freq<- na.omit(Freq)
    print(Freq)
   
    
    abmean<-tapply(behavior,PhaseX,mean,na.rm=T)
    pmean<-c(round(abmean,3))
    pmean<-as.data.frame(pmean)
    
    writeLines("-----------Mean------------------------------")
    len=length(pmean)
     if (anyNA.data.frame(abmean) & len > 1){  
      
      pmean<-pmean[c(-1)]
    } 
    
    if (len <2) { pmean <- na.omit(pmean)} 
     
     colnames(pmean)<- c("M")
      

    print(pmean)
    
    tmean<-tapply(behavior,PhaseX,mean,trim=.1,na.rm=T)
    tmean<-c(round(tmean,3))
    writeLines("-----------10% Trim Mean---------------------")
    tmean<-as.data.frame(tmean)
    len=length(tmean)
    if (anyNA.data.frame(tmean) & len > 1)   {
      
      tmean<-tmean[c(-1)]
    } 
    if (len <2) { tmean <- na.omit(tmean)} 
    colnames(tmean)<- c("tM") 
   
     print(tmean)
    
    abmedian<-tapply(behavior,PhaseX,median,na.rm=T)
    pmedian<-c(round(abmedian,3))
    writeLines("----------Median-----------------------------")
   
     pmedian<-as.data.frame(pmedian)
     len<-length(pmedian)
    if (anyNA.data.frame(pmedian) & len > 1)  {
      
      pmedian<-pmedian[c(-1)]
    }
     if (len <2) { pmedian <- na.omit(pmedian)} 
     colnames(pmedian)<- c("Mdn") 
    print(pmedian)
    
    absd<-tapply(behavior,PhaseX,sd,na.rm=T)
    
    absd<-c(round(absd,3))
    absd<-as.data.frame(absd)
    len<-length(absd)
    writeLines("-----------Standard Deviation----------------")
    
    
    if (anyNA.data.frame(absd) & len >1 ) {
      
      absd<-absd[c(-1)]
    }
   if (len <2) { absd <- na.omit(absd)} 
    colnames(absd)<- c("SD") 
    print(absd)
    
   cv<-absd/pmean
   
   #cv<-c(round(cv,3))
 
    #cv<-as.data.frame(cv)
    
  #len<-length(cv)
  #if (anyNA.data.frame(cv & len >1 ) )  {
   
     #cv<-cv[c(-1)]
    
#  } 
    writeLines("------------Coefficient of Variation---------")
    
    #if (len <2) { cv<- na.omit(cv)} 
    colnames(cv)<- c("CV") 
    
     print(round(cv,digits=3))
    
    writeLines("---------Range-------------------------------")
    Phasex<- as.character(PhaseX[PhaseX!=""])
    Phasex<-na.omit(Phasex)
    Behavior<-na.omit(behavior)
    Range<-tapply(Behavior,Phasex,range,na.rm=TRUE)
    
    
    len<-length(Range)
    
  
   # if  (len>2 )   {
      
     # Range<-Range[c(-1)]
   # }
    Range<-as.data.frame(Range)
    
    if (len <2) { Range <- na.omit(Range)} 
    colnames(Range)<- c("Range") 
      
     print(Range)
    
   
    
    writeLines("---------Interquartile Range-----------------")
    abiqr<-tapply(behavior,PhaseX,IQR,na.rm=T)
    
    
    abiqr<-as.data.frame(abiqr)
    len<-length(abiqr)
    #if (anyNA.data.frame(abiqr )  )  {
    #  abiqr<-abiqr[c(-1)]
      
    #}
    
    if (len <2) { abiqr <- na.omit(abiqr)} 
    colnames(abiqr)<- c("IQR") 
    
    print(abiqr)
    
   
    writeLines("---------Quantiles---------------------------")
    Quantiles<-(tapply(behavior,PhaseX,quantile,na.rm=T))
      
    Quantiles<-as.data.frame.list( Quantiles)
   len<-length(Quantiles)
     if (anyNA.data.frame(Quantiles ) & len > 1)  {
       
      Quantiles<-Quantiles[c(-1)]
    } 
    
   if (len <2) { Quantiles <- na.omit(Quantiles)} 
    
    print(Quantiles)
     
    
    
    layout(rbind(1,2), heights=c(6,1))
    boxplot(behavior~PhaseX,xlab="Phase")
    options(warn=-0)
  }
