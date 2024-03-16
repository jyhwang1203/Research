
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("nloptr","MASS")
ipak(pkg)
MVO <- function(eb=eb,TMP=TMP,lamda=lamda,coeff=coeff,wl=wl,n=n){
  coeff=coeff
  
  omega  <- TMP %>% dplyr::select(GROWTH,INF,RINTEREST,CREDIT,FX) %>% cov * 12^0.5
  # w<-rep(1/12,12)
  # lamda<- 0.99
  
  objective_mvo = function(w) {
    lamda <- lamda
    #w<-rep(1/7,7)
    #w<-  c(0.7,0,0.3,0,0,0,0)
    obj = (1-lamda)*(t(w)%*%coeff[]-eb)%*%t(t(w)%*%coeff[]-eb)+lamda*(t(w)%*%coeff[,]-eb)%*%omega%*%t(t(w)%*%coeff[]-eb)
    
    return(obj)
  }
  
  #제약조건     
  heq.objective = function(w) {
    sum <- numeric(1)
    sum[1] = sum(w)-1
    #sum[2] = w[1]-0.5
    return( sum )
  }
  
  hin.objective <- function(w) {
    h <- numeric(13)
    # h[1 ] <- t(w) %*%rep(1/7,7) - 0.05
    
    #  h[13 ] <- -((t(w) %*% omega %*% w)*12)^0.5+0.05
    # h[3] <- (rep(1/7,7))%*%(coeff[c(1:7),-1])
    
    #h[8] <-  w[2]-.1
    
    h[1 ] <- w[1] 
    h[2 ] <- w[2]
    h[3 ] <- w[3]
    h[4 ] <- w[4]
    h[5 ] <- w[5]
    h[6 ] <- w[6]        
    h[7 ] <- w[7]  
    h[8 ] <- w[8] 
    h[9 ] <- w[9] 
    h[10] <- w[10] 
    h[11] <- w[11] 
    h[12] <- w[12]
    h[13] <- w[13]
    
    return(h)
  }
  
  result = slsqp( x0 = rep(1/n,n),
                  fn = objective_mvo,
                  hin = hin.objective,
                  heq = heq.objective,
                  control = list(xtol_rel = 1e-8),
                  lower = rep(wl, n),
                  upper = rep(1, n))
  #결과값
  res2 <- result$par %>%round(4)
  
  
  
  return(res2)
}
#retm <- RAWDATA %>% trans_rt("quarter")%>%dt_trans()
s <- which((retm$STD_DT)=="2009-12-31")
n <-(retm%>%filter(STD_DT>"2010-01-01")%>%nrow-1)
tmp <- list()

res <- t(sapply(c(1:n),function(t){
  #  t =5
  STDDT1 <- retm$STD_DT[s+t-1]
  
  # lamda <- 0.99
  TMP  <<-  retm %>% filter(STD_DT<STDDT1&STD_DT>STDDT1-years(10))
  # REG1 <-  lm(MSUS ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary 
  REG0 <-  lm(MSUS  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG1 <-  lm(MSUS  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG1 <-  lm(MSUS ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG2 <-  lm(EM~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG3 <-  lm(KOSPI~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG4 <-  lm(USGOVT ~GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG5 <-  lm(USIG~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG6 <-  lm(USHY  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG7 <-  lm(KRBONDH ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG8 <-  lm(EMBOND ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG9 <-  lm(WRINFRA ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG10 <- lm(WREPRA ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG11 <- lm(HFRI  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG12 <- lm(WTI ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG13 <- lm(GOLD~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  
  coeff <- REG0%>%  full_join(REG1,by=c("Var1","Var2")) %>%  full_join(REG2,by=c("Var1","Var2"))%>%  full_join(REG3,by=c("Var1","Var2"))%>%  full_join(REG4,by=c("Var1","Var2"))%>%  full_join(REG5,by=c("Var1","Var2")) %>%  
    full_join(REG6,by=c("Var1","Var2"))%>%  full_join(REG7,by=c("Var1","Var2"))%>%  full_join(REG8,by=c("Var1","Var2"))%>%  full_join(REG9,by=c("Var1","Var2"))%>%  full_join(REG10,by=c("Var1","Var2"))%>%
    full_join(REG11,by=c("Var1","Var2"))%>%  full_join(REG12,by=c("Var1","Var2"))%>%  full_join(REG13,by=c("Var1","Var2")) %>% filter(Var2=="Estimate")
  coeff[is.na(coeff)]<-0
  coeff <-coeff[-1,-c(1,2,3)]%>%t%>%as.matrix
  colnames(coeff)<-c("GROWTH","INF","RINTEREST","CREDIT","FX")
  
  
    TMP2 <-retm %>% filter(STD_DT<STDDT1&STD_DT>STDDT1-years(10))
  REG1 <-  lm(WORLD  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG2 <-  lm(WORLD  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG3 <-  lm(WRBOND  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  REG4 <-  lm(AL  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP2)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame
  fcoeff <- REG1 %>%  full_join(REG2,by=c("Var1","Var2"))%>%  full_join(REG3,by=c("Var1","Var2"))%>%full_join(REG4,by=c("Var1","Var2"))%>%
    filter(Var2=="Estimate")
  fcoeff[is.na(fcoeff)]<-0
  fcoeff <-fcoeff[-1,-c(1,2)]%>%t%>%as.matrix
  colnames(fcoeff)<-c("GROWTH","INF","RINTEREST","CREDIT","FX")
  
  
  
  #eb <<-(fcoeff[1,]*0.6+ fcoeff[3,]*0.4)
  eb <<-(fcoeff[2,]*0.5+ fcoeff[3,]*0.3+ fcoeff[4,]*0.2)
  #eb <<- fcoeff[4,]
  
  n <- coeff %>% nrow
  wei<- data.frame(as.data.frame(t(MVO(eb,TMP,0.99,coeff,0.0,n))))
  tmp$wei <- cbind(STDDT1,wei)
  colnames(wei) <-c("MSUS","EM","KOSPI","USGOVT","USIG","USHY","KRBONDH","EMBOND","WRINFRA","WREPRA","HFRI","WTI","GOLD")
  TMP2 <-retm %>% filter(STD_DT<STDDT1&STD_DT>STDDT1-years(10))
  ret2<- retm%>%filter(STD_DT==((STDDT1)))%>%melt(id.vars="STD_DT")
  ret3 <- wei%>%melt%>%left_join(ret2,by="variable")
  ret3<- sum(ret3[,2]*ret3[,4])
  tmp$ret <- data.frame(STDDT1,ret3)
  tmp$exp <- (wei%>%as.matrix)%*%coeff
  tmp$eb <-eb
  return(tmp)
}
))

retm 
#do.call(rbind,res[,2])%>%View
RT_MACRO <- do.call(rbind,res[,2])
FACTORRT <- do.call(rbind,res[,3])
wei_macro <- do.call(rbind,res[,1])
colnames(wei_macro)[-1]  <-c("MSUS","EM","KOSPI","USGOVT","USIG","USHY","KRBONDH","EMBOND","WRINFRA","WREPRA","HFRI","WTI","GOLD")
FACTOREXP <- do.call(rbind,res[,4])
# RT_MF<- retm%>%dplyr::select(STD_DT,GROWTH,INF,CREDIT,RINTEREST,FX)%>%cuml
# PA_MF<- retm%>%dplyr::select(STD_DT,GROWTH,INF,CREDIT,RINTEREST,FX)%>%PA
ret3 <- RT_MACRO %>%left_join(retm%>%filter(STD_DT>="2010-01-01")%>%dplyr::select(STD_DT,WORLD,AL),by=c("STDDT1"="STD_DT"))
ret3[,-1] <- ret3[,-1]%>%round(4)
RET_MACRO <- ret3%>%na.omit
#ret3 <- cbind(ret2,retm%>%filter(STD_DT>="2010-01-01")%>%select(STD_DT,WRBOND,WORLD,AL)%>%dt_trans%>%mutate(BM=(0.5*WORLD+0.3*WRBOND+0.2*AL))%>%.[-159,6])
colnames(RET_MACRO)<- c("STD_DT","MP","WORLD","BM")
RT_MACRO <- data.frame(STD_DT=RET_MACRO$STD_DT,cumprod(1+RET_MACRO[,-1]))


g3 <- RT_MACRO %>% melt(id.vars="STD_DT")%>%ggplot(aes(STD_DT, value, col = variable)) +             
  geom_line(size=1)+ 
  ggtitle("누적수익률") +
  theme(legend.text = element_text(size=15))

TARGETEXP  <- rbind( (wei_macro[  wei_macro%>%nrow,-1]%>%as.matrix)%*%coeff,eb)
rownames(TARGETEXP)<-c("MP","BM")

g4 <- TARGETEXP%>%reshape2::melt()%>%ggplot(aes(x=Var2, y=value, col = Var1,fill=Var1)) +             
  geom_bar(width = 1, stat = "identity",position = "dodge")+
  theme(legend.text = element_text(size=15))


wei_macro%>%View
RET_STEP <- RET_MACRO
WEI_STEP <- wei_macro
FACTOREXP_STEP <-FACTOREXP
grid.arrange( g4,g2,ncol=2)
RT_MACRO
RET_REG%>%PA
RET_STEP%>%PA
res1<-RET_REG%>%cuml%>%left_join(RET_STEP%>%dplyr::select(STD_DT,MP)%>%cuml,by="STD_DT")
res2<-RET_REG%>%left_join(RET_STEP%>%dplyr::select(STD_DT,MP),by="STD_DT")%>%PA
res1%>%cplot
RET_REG%>%left_join(RET_STEP%>%dplyr::select(STD_DT,MP),by="STD_DT")%>%PA
