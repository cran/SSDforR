metareg <-
  function(es,v){
    m0 <- mareg( es ~ 1, var = v, method = "REML"
                 )
    print(summary(m0))
    funnel(m0)
  }
