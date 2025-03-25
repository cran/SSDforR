ABdescrip <-
  function(behavior,PhaseX){
   
    
    

    writeLines("-----------n-------------")
    t1<-table (PhaseX)
    t1<-t1[c(-1)]
    print(t1)

    abmean<-tapply(behavior,PhaseX,mean,na.rm=T)
    pmean<-c(round(abmean,3))
    writeLines("-----------mean-------------")
    pmean<-pmean[c(-1)]
    print(pmean)
    
    tabmean<-tapply(behavior,PhaseX,mean,trim=.1,na.rm=T)
    tmean<-c(round(tabmean,3))
    writeLines("-----------10% trim mean-------------")
    tmean<-tmean[c(-1)]
    print(tmean)
    
    abmedian<-tapply(behavior,PhaseX,median,na.rm=T)
    pmedian<-c(round(abmedian,3))
    writeLines("----------median------------")
    pmedian<-pmedian[c(-1)]
    print(pmedian)
    
    absd<-tapply(behavior,PhaseX,sd,na.rm=T)
    psd<-c(round(absd,3))
    writeLines("------------SD--------------")
    psd<-psd[c(-1)]
    print(psd)
    
    cv<-psd/pmean
    pcv<-c(round(cv,3))
    writeLines("------------CV--------------")
    pcv<-pcv[c(-1)]
    print(pcv)
    
    writeLines("---------range----------")
    abrange<-tapply(behavior,PhaseX,range,na.rm=T)
    #prange<- do.call("rbind", abrange)
    
    abrange<-abrange[c(-1)]
   
    
    print(abrange)
    
    
    writeLines("---------iqr----------")
    abiqr<-tapply(behavior,PhaseX,IQR,na.rm=T)
    
    
    abiqr<-abiqr[c(-1)]
    
    #cat(sprintf(pret),"\n") 
    print(abiqr)
    
   
    writeLines("---------quantiles----------")
    abquan<-(tapply(behavior,PhaseX,quantile,na.rm=T))
      
    abquan<-abquan[c(-1)]
    print(abquan,quote=FALSE)
     
    #do.call("rbind", abquan)
    
    layout(rbind(1,2), heights=c(4,1))
    boxplot(behavior~PhaseX)
    
  }
