Effectsize <-
function(behavior,phaseX,v1,v2) {
  mean1 <-tapply(behavior,phaseX,mean,na.rm=T)
  s1 <-tapply(behavior,phaseX, sd, na.rm=T)
 
  options(scipen=999)
  
  tphase<-table(phaseX)
  n1<- tphase[names(tphase)==v1]
  n2<- tphase[names(tphase)==v2]
  
  n1<-n1-1 
  n2<-n2-1
  totaln<-n1+n2
  DIFF<-mean1[names(mean1)==v1]-mean1[names(mean1)==v2]
 
  S<-sqrt(((n1*(s1[names(s1)==v1]^2)+(n2*s1[names(s1)==v2]^2))/totaln))
 
  CD<-(DIFF/S)

   V<-S^2
 
  cm=gamma((totaln/2))/(sqrt(totaln/2)*gamma((totaln-1)/2))
  G<-CD*cm
   t1<-table(phaseX)
  tmaxA<-t1[names(t1)==v1]
  startA<-match(v1,phaseX)
  endA<-tmaxA+startA-1
  A<-behavior[startA:endA]
  PA<-phaseX[startA:endA]
         
  tmaxA<-t1[names(t1)==v2]
  startA<-match(v2,phaseX)
  endA<-tmaxA+startA-1
  B<-behavior[startA:endA]
   PB<-phaseX[startA:endA]
  
  
IV<-c(PA,PB)
  DV<-c(A,B)
  
  
 reg1<- summary(lm(DV~IV))
  rvalue2<-reg1$r.squared
  rvalue<-sqrt(reg1$r.squared)
  
  
  DIFF<-mean1[names(mean1)==v2]-mean1[names(mean1)==v1]
  S<-s1[names(s1)==v1]
  es<-(DIFF/S)
  nt=n1+n2+2
  nx=n1+1
  ny=n2+1
  l1<-c("small effect size: <.87")
  l2<-c("medium effect size: .87 to 2.67 ")
  l3<-c("large effect size: >2.67")
  writeLines(" ")
  writeLines("****************************************************************************")
  writeLines("Note: the ES index, d-index, and  Hedges's g index are unreliable")
  writeLines("if any phase is autocorrelated or has a trend.")
  writeLines("Use the TauU test if any phase is autocorrelated.")
  writeLines("If no autocorrelation exists in any phase, but a trend exists,") 
  writeLines("use the TauU or G-index test.")
  writeLines("****************************************************************************")
  writeLines(" ")
  writeLines(l1)
  writeLines(l2)
  writeLines(l3)
  writeLines("********************************************************")
  writeLines("********************ES**********************************")
  
  es1<-round(es,5)
  pes1<-c("ES=        ",abs(es1))
  eschange=pnorm(es)-.5
  l5<-c("% change=",round(eschange,4)*100)
  print(c(pes1,l5))
  
  
  des<-abs(es1)
  esci<- d.ci(des,n2=ny,n1=nx)
  writeLines(" ")
  print(esci)
  
  
  writeLines("*****************d-index*******************************")
  cd1<-(round(abs(CD),5))
  retcd<-cd1
  pcd1<-c("d-index=   ",cd1)
  dchange=pnorm(CD)-.5
  l6<-c("% change=",round(dchange,4)*100)
  
 dci<- d.ci(CD,n=nt,n2=ny,n1=nx)

  print(c(pcd1,l6))
 
  writeLines(" ")
  print(dci)
  
 writeLines("*****************Hedges's g****************************")
  hchange=pnorm(G)-.5
G1<-(round(abs(G),5))
PG1<-c("Hedges's g=",G1)
l7<-c("% change=",round(hchange,4)*100)

print(c(PG1,l7))
Gci<- d.ci(G1,n=nt,n2=ny,n1=nx)
writeLines(" ")
print(Gci)
  writeLines("*****************Pearson's r***************************")
  print(round(rvalue,3))
  writeLines("*****************R-squared*****************************")
  print(round(rvalue2,3))
  
  SE<-sqrt((ny+nx)/(ny*nx)+((CD^2))/(2*(ny+nx-2))*(ny+nx)/(nx+ny-2))
  
  ret<-retrodesign(CD, SE, alpha = 0.05, df =totaln, n.sims = 10000)
  writeLines(" ")
  writeLines("*************************************************************************************************")
  writeLines("Type M (Magnitude) and Type S (Sign) Errors by Andrew Gelman and John Carlin (2014)")
  writeLines("In the graph, the dotted line is the actual effect size, and the full line is where")
  writeLines("the statistic becomes statistically different from 0, given the standard error.")
  writeLines("The grayed-out points aren't statisticaly significant, the squares are type M errors,") 
  writeLines("and the triangles are type S Errors. Type M Errors shold be close to one and")
  writeLines("Type S Errors close to zero.")
  writeLines("*************************************************************************************************")
  writeLines(" ")
  power<-ret$power[1]
  sign<-ret$type_s[1]
  spercent<-sign*100
  spercent<-round(spercent,1)
  magnitude<-ret$type_m[1]
  pret<-c("Power=",round(power,3))
  mret<-c("Type M (Magnitude) Errors=",round(magnitude,3))
  sret<-c("Type S Sign) Errors=",round(sign,3),spercent,"percent")
  cat(sprintf(pret),"\n") 
  cat(sprintf(mret),"\n") 
  cat(sprintf(sret),"\n") 
  writeLines("*************************************************************************************************")
  writeLines(" ")

  #dev.off()
  par(mar = rep(2, 4))
sim_plot(CD, SE, alpha = 0.05, df =totaln, n.sims = 10000,gg=F)
 
  
  a<-readline("(s)ave, (a)ppend, or (n)either results? (s/a or n) ")
  
  
  ES<-CD
 
  ES=data.frame(ES)
  
  if (a=="s")
    {Label<-readline("Enter a behavior variable label ")
    ES<-data.frame(ES,V,Label)
  
    write.csv(ES,file = file.choose(new = TRUE),row.names=FALSE)
    #write.csv(ES,file = tclvalue(tkgetSaveFile()),row.names=FALSE)
  
    
   } 

   if (a=="a")
   { Label<-readline("Enter a behavior variable label ")
   ES<-CD
     ES<-data.frame(ES,V,Label)
     writeLines("*****************open file to append to***************************")
     
     effA<-read.table(file.choose(),header=TRUE,sep=',') 
            out=rbind(effA,ES)
            writeLines(" ")
            writeLines(" ")
            writeLines(" ")
            writeLines("*****************save appended file***************************")  
            
            
  # write.csv(out,file = tclvalue(tkgetSaveFile()),row.names=FALSE)}
  write.csv(out,file = file.choose(new = TRUE),row.names=FALSE)}
  
  
}
