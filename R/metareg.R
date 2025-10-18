metareg <-
  function(es, v){
    options(warn=-1)
     m0 <- mareg(es~1, var=v , method = "REML")
    
    
    print(summary(m0))
   print(confint(m0))
    
   layout(rbind(1,2), heights=c(6,1))
    forest(m0)
    title(main="Forest Graph of Effect Sizes")
    options(warn=-0)
  }
