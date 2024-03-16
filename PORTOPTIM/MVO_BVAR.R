##
  MVO_BVAR  <- function(mu,cov,trate,n,wl,vol){
 
    vol2<<-vol
    wl2<<- wl
   objective_mvo = function(w) {
    obj =  0.5*((t(w) %*% cov %*% w)) -  t(w) %*% t(mu)
    return(obj)
  }
  
  
  heq.objective = function(w) {
   
   sum <- numeric(1)
    sum[1] = sum(w)-1
    #sum[2 ] = -((t(w) %*% cov %*% w))^0.5 + vol
    return( sum )
  }

  hin.objective <- function(w) {
      h <- numeric(12)
      h[1 ] <- w[1]
      h[2 ] <- w[2]
      h[3 ] <- w[3]
      h[4 ] <- w[4]
      h[5 ] <- w[5]
      h[6 ] <- w[6]
      h[7 ] <-  -w[3]-w[4] + 0.3
      h[8 ] <- t(w) %*% t(mu) - trate
      h[9 ] <- -((t(w) %*% cov %*% w))^0.5 + vol
      h[10 ] <-  -w[6]-w[5]+0.30
      h[11 ] <- -w[3] + 0.1
      h[12 ] <- -w[1] + 0.1
      return( h )
      }
    
  
  result = slsqp(  x0 = rep(1/(n),(n)),
                   fn = objective_mvo,
                   hin = hin.objective,
                   heq = heq.objective,
                   control = list(xtol_rel = 1e-8),lower = rep(0.04, n),
                   upper = rep(0.5, n))
  
 
  #결과값
  
 
  names(result$par) <-c("글로벌주식","한국주식","글로벌채권","한국채권","부동산산","인프라라")
  result$par%>%t%>%as.data.frame %>%round(4)  
  
  }
  