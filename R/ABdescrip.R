ABdescrip <-
  function(behavior,PhaseX){
   
    options(warn=-1)
    
writeLines(" ")
    writeLines("-----------Number of Observations------------")
    Freq<-table (PhaseX)
   Freq<-Freq[c(-1)]
    Freq<-as.data.frame(Freq)
    len=length(Freq)

    if (len > 1 )  {
   colnames(Freq)<- c("Phase","Freq") 
  
  }  else 
   {  colnames(Freq)<- c("Freq") 
     } 
    print(Freq)
   
    abmean<-tapply(behavior,PhaseX,mean,na.rm=T)
    pmean<-c(round(abmean,3))
    writeLines("-----------Mean------------------------------")
    pmean<-pmean[c(-1)]
    print(pmean)
    
    tabmean<-tapply(behavior,PhaseX,mean,trim=.1,na.rm=T)
    tmean<-c(round(tabmean,3))
    writeLines("-----------10% Trim Mean---------------------")
    tmean<-tmean[c(-1)]
    print(tmean)
    
    abmedian<-tapply(behavior,PhaseX,median,na.rm=T)
    pmedian<-c(round(abmedian,3))
    writeLines("----------Median-----------------------------")
    pmedian<-pmedian[c(-1)]
    print(pmedian)
    
    absd<-tapply(behavior,PhaseX,sd,na.rm=T)
    psd<-c(round(absd,3))
    writeLines("-----------Standard Deviation----------------")
    psd<-psd[c(-1)]
    print(psd)
    
   cv<-psd/pmean
    pcv<-c(round(cv,3))
  
    writeLines("------------Coefficient of Variation---------")
    
    print(pcv)
    
    writeLines("---------Range-------------------------------")
    Range<-tapply(behavior,PhaseX,range,na.rm=T)
    
    
    Range<-Range[c(-1)]
    Range<-as.data.frame(Range)
  
     print(Range,quotes=F)
    
   
    
    writeLines("---------Interquartile Range-----------------")
    abiqr<-tapply(behavior,PhaseX,IQR,na.rm=T)
    
    
    abiqr<-abiqr[c(-1)]
    
    
    print(abiqr)
    
   
    writeLines("---------Quantiles---------------------------")
    Quantiles<-(tapply(behavior,PhaseX,quantile,na.rm=T))
      
    Quantiles<-Quantiles[c(-1)]
    Quantiles<-as.data.frame.list( Quantiles)
   
    
    print(Quantiles)
     
    
    
    layout(rbind(1,2), heights=c(4,1))
    boxplot(behavior~PhaseX,xlab="Phase")
    options(warn=-0)
  }
