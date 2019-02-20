metareg <-
  function(es, v){
    m0 <- mareg( es ~ 1, var = v, method = "REML"
                 )
    print(summary(m0))
   print(confint(m0, digets=2))
    forest(m0)
  }
