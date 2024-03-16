##
  MVO_VOLTARGET  <- function(mu,cov,trate,n,wl,vol){
    vol2<<-vol
    wl2<<- wl
   objective_mvo = function(w) {
    obj =  1*((t(w) %*% cov %*% w)) -  t(w) %*% t(mu)
    return(obj)
  }
  
  
  heq.objective = function(w) {
   
   sum <- numeric(2)
    sum[1] = sum(w)-1
    sum[2 ] = -((t(w) %*% cov %*% w))^0.5 + vol
    return( sum )
  }

  hin.objective <- function(w) {
      h <- numeric(7)
      h[1 ] <- w[1]
      h[2 ] <- w[2]
      h[3 ] <- w[3]
      h[4 ] <- w[4]
      h[5 ] <- w[5]
      h[6 ] <- w[6]
      h[7 ] <-  -w[3]-w[4] + 0.4
     # h[8 ] <- t(w) %*% t(mu) - trate
    #  h[8 ] <- -((t(w) %*% cov %*% w))^0.5 + vol
      return( h )
      }
    
  
  result = slsqp(  x0 = rep(1/(n),(n)),
                   fn = objective_mvo,
                   hin = hin.objective,
                   heq = heq.objective,
                   control = list(xtol_rel = 1e-8),lower = rep(wl, n),
                   upper = rep(0.3, n))
  
 
  #결과값
  
 
  names(result$par) <-c("글로벌주식","한국주식","글로벌채권","한국채권","대체투자","원자재")
  result$par%>%t%>%as.data.frame %>%round(4)  
  
  }

  weiv1<- MVO_VOLTARGET(mu24,12*cov24,trate=trate,n=6,wl=0.03,0.16)%>%as.matrix()
  weiv2<- MVO_VOLTARGET(mu24,12*cov24,trate=trate,n=6,wl=0.03,0.14)%>%as.matrix()
  weiv3<- MVO_VOLTARGET(mu24,12*cov24,trate=trate,n=6,wl=0.03,0.12)%>%as.matrix()
  weiv4<- MVO_VOLTARGET(mu24,12*cov24,trate=trate,n=6,wl=0.03,0.10)%>%as.matrix()
  weiv5<- MVO_VOLTARGET(mu24,12*cov24,trate=trate,n=6,wl=0.03,0.08)%>%as.matrix()
  weiv6<- MVO_VOLTARGET(mu24,12*cov24,trate=trate,n=6,wl=0.03,0.06)%>%as.matrix()
 
  G1VOL <- weiv1%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표변동성=","16%")) 
  G2VOL <- weiv2%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표변동성=","14%")) 
  G3VOL <- weiv3%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표변동성=","12%")) 
  G4VOL <- weiv4%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표변동성=","10%")) 
  G5VOL <- weiv5%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표변동성=","8%")) 
  G6VOL <- weiv6%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표변동성=","6%")) 
  grid.arrange(G1VOL,G2VOL,G3VOL,G4VOL,G5VOL,ncol=5)
  
  
  temp <- rbind(
    c((weiv1) %*% t(mu24), ((weiv1) %*%(12*cov24)%*% t(weiv1))^0.5),
    c((weiv2) %*% t(mu24), ((weiv2) %*%(12*cov24)%*% t(weiv2))^0.5),
    c((weiv3) %*% t(mu24), ((weiv3) %*%(12*cov24)%*% t(weiv3))^0.5),
    c((weiv4) %*% t(mu24), ((weiv4) %*%(12*cov24)%*% t(weiv4))^0.5),
    c((weiv5) %*% t(mu24), ((weiv5) %*%(12*cov24)%*% t(weiv5))^0.5),
    c((weiv6) %*% t(mu24), ((weiv6) %*%(12*cov24)%*% t(weiv6))^0.5))%>%data.frame
  
  colnames(temp) <- c("mean","vol")
  temp <-temp %>% mutate(SR=(mean)/vol)%>%round(4)
  
  
  voltarget <-  cbind(
    voltarget= c("16%","14%","12%","10%","8%","6%"),
    temp,
    rbind(
      weiv1%>%data.frame,
      weiv2%>%data.frame,
      weiv3%>%data.frame,
      weiv4%>%data.frame,
      weiv5%>%data.frame,
      weiv6%>%data.frame))%>%as.data.frame()

  voltarget
  # write.xlsx(voltarget ,"c:/work/MP.xlsx", sheetName="voltarget",append=F)
#  G6<- MVO_VOLTARGET(mu23,12*cov,trate=0,n=6,wl=0.03,0.14)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
#    ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
#  G7<- MVO_VOLTARGET(mu,12*cov,trate=0,n=6,wl=0.03,0.12)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
#    ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
#  G8<- MVO_VOLTARGET(mu,12*cov,trate=0,n=6,wl=0.03,0.10)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
#    ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
#  G9<- MVO_VOLTARGET(mu,12*cov,trate=0,n=6,wl=0.03,0.08)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
#    ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
#  G10<- MVO_VOLTARGET(mu,12*cov,trate=0,n=6,wl=0.03,0.06)%>%data.frame%>%t%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+
#    ggtitle(paste0("wl=",wl2,"vol=",vol2)) 
#  grid.arrange(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,ncol=5)
#  

 
  
  
  