metaregi <-
  function(es,i, v){
    m0 <- mareg( es ~ i, var = v, method = "REML"
    )
    print(summary(m0))
    print(confint(m0, digets=2))
    forest(m0)
  }