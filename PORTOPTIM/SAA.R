  tg <- RAWDATA%>%filter(variable=="WORLDT"|variable=="WRBOND")%>%na.omit%>%dcast(STD_DT~variable)%>%
  trans_rt("month")%>%dt_trans%>%filter(STD_DT>"2000-01-01")%>%
  mutate(BM1=0.7 *WORLDT+0.3*WRBOND)%>%
  mutate(BM2=0.35*WORLDT+0.65*WRBOND)%>%
  mutate(BM3=0.2 *WORLDT+0.8*WRBOND)%>%PA%>%data.frame()



##
MVO_VOLTARGET  <- function(mu,cov,trate,n,wl,vol,CT){
  vol2<<-vol
  wl2<<- wl
  objective_mvo = function(w) {
    obj =  0.5*((t(w) %*% cov %*% w)) -  t(w) %*% t(mu)
    return(obj)
  }
  
  
  heq.objective = function(w) {
    
    sum <- numeric(1)
    sum[1] = sum(w)-1
   # sum[2 ] = -((t(w) %*% cov %*% w))^0.5 + vol
    return( sum )
  }
  
  hin.objective <- function(w) {
    h <- numeric(9)
    h[1 ] <- w[1]
    h[2 ] <- w[2]
    h[3 ] <- w[3]
    h[4 ] <- w[4]
    h[5 ] <- w[5]
   
    h[6 ] <- -(w[5]+w[6])+0.15
 
   # eval(parse(paste0(text="h[8 ] <- -(w[2]+w[4])+",CT)))
    h[7 ] <- (w[3]+w[4]) - CT
    h[8 ] <- t(w) %*% t(mu) - trate
    h[9 ] <- -((t(w) %*% cov %*% w))^0.5 + vol
   # h[10] <- -(w[1]+w[3]+w[5]+w[6]+w[7])+0.7
    return( h )
  }
  
  
  result = slsqp(  x0 = rep(1/(n),(n)),
                   fn = objective_mvo,
                   hin = hin.objective,
                   heq = heq.objective,
                   control = list(xtol_rel = 1e-8),lower = rep(wl, n),
                   upper = rep(0.4, n))
  
  
  #결과값
  
  names(result$par) <-c("MSKRT","WORLDT","KRBONDH","USBOND","WREPRA","WRINFRA")
  #names(result$par) <-c("한국주식","한국채권","글로벌주식","글로벌채권","신흥국채권","부동산","인프라")
  result$par%>%t%>%as.data.frame %>%round(4)  
  
}
#RT[1,4] <- RT[1,4]+0.005
#w = rep(1/(6),(6))
mu24 <- RT[1,c(-5,-8)]-1
#-((t(w) %*% cov24 %*% w))^0.5
cov24 <- COR_E[c(-5,-8),c(-5,-8)]
cov24 <- vcov(run)[c(-5,-8),c(-5,-8)]*4
mu24
VOL
MVO_VOLTARGET(mu24,cov24,trate= tg["MEAN","BM1"] ,n=6,wl=0.04,tg["VOL","BM1"],CT=0.3)%>%as.matrix()
MVO_VOLTARGET(mu24,cov24,trate= tg["MEAN","BM2"] ,n=6,wl=0.04,tg["VOL","BM2"] ,CT=0.65 )%>%as.matrix()
MVO_VOLTARGET(mu24,cov24,trate= tg["MEAN","BM3"] ,n=6,wl=0.04,tg["VOL","BM3"] ,CT=0.80 )%>%as.matrix()
weiv1<- MVO_VOLTARGET(mu24,cov24,trate= tg["MEAN","BM1"] ,n=6,wl=0.04,tg["VOL","BM1"],CT=0.3)%>%as.matrix()
weiv2<- MVO_VOLTARGET(mu24,cov24,trate= tg["MEAN","BM2"] ,n=6,wl=0.04,tg["VOL","BM2"] ,CT=0.65 )%>%as.matrix()
weiv3<- MVO_VOLTARGET(mu24,cov24,trate= tg["MEAN","BM3"] ,n=6,wl=0.04,tg["VOL","BM3"] ,CT=0.80 )%>%as.matrix()

wei <-rbind(weiv1,weiv2,weiv3)
wei
G1VOL <- weiv1%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표변동성=","16%")) 
G2VOL <- weiv2%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표변동성=","14%")) 
G3VOL <- weiv3%>%as.data.frame()%>% melt()%>% ggplot(aes(variable, value,col=variable,fill=variable))+geom_bar(stat = "identity")+   ggtitle(paste0("최소자산비중=",wl2,"목표변동성=","12%")) 

grid.arrange(G1VOL,G2VOL,G3VOL,ncol=3)
  
xlsx::write.xlsx(wei    ,"c:/work/SAA.xlsx", sheetName="wei",append=F)
xlsx::write.xlsx(COR_E ,"c:/work/SAA.xlsx", sheetName="COR_E",append=T)
xlsx::write.xlsx(COR_H ,"c:/work/SAA.xlsx", sheetName="COR_H",append=T)
xlsx::write.xlsx(VOL   ,"c:/work/SAA.xlsx", sheetName="VOL",append=T)
xlsx::write.xlsx(tg   ,"c:/work/SAA.xlsx", sheetName="BM",append=T)

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




