ipak<- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

pkg <-c("demography","reshape2","dplyr","reshape2","plyr","MortCast","MortalityLaws","vars",
        "nloptr","tseries","urca",'zoo','StMoMo',"TSA","ggplot2","xlsx","gridExtra")
ipak(pkg)

cusum <- function(data) {
  fit <-auto.arima(data)
  autoplot(forecast(fit))
  fit$sigma2
  
  demean <- diff(data) %>%mean
  z <- diff(data)
  e <- fit$residuals
  e   %>%length
  tt  <- fit$x %>% length
  ttt <- tt-1
  
  res2 <- sapply(1:(e%>%length),function(t){
    ((abs(e[1:t]^2 %>%sum - sum(e^2)* t/(e%>%length)))/
       ((e%>%length*fit$sigma2)^0.5))%>%return
  })
  return(res2)
}


ages.fit <- 0:99

# 
# kt <- data.frame(LCfit$kt%>%as.numeric ,
#            LCfitm$kt%>%as.numeric,
#            LCfitf$kt%>%as.numeric
#            )

source("c:/HWANG/HJPROJECT/FUNCTION/mortality.R")
LC <- lc(link = "logit")
wxt <- genWeightMat(ages = ages.fit, years = BASEKR$years, clip = 3)

LCfit  <- StMoMo::fit(LC, data = BASEKR, ages.fit = ages.fit)
LCfitm  <- StMoMo::fit(LC, data = BASEKRM, ages.fit = ages.fit)
#LLfitm <-  llmodel(BASEKRM,BASEKR)
LCfitf  <- StMoMo::fit(LC, data = BASEKRF, ages.fit = ages.fit)
#LLfitf <-  llmodel(BASEKRF,BASEKR)

LCfor  <- forecast::forecast(LCfit, h = 50)
LCform <- forecast::forecast(LCfitm, h = 50)
LCforf <- forecast::forecast(LCfitf, h = 50)
MXT    <- cbind(LCfitm$Dxt/LCfitm$Ext,LCform$rates)
FXT    <- cbind(LCfitf$Dxt/LCfitf$Ext,LCforf$rates)
TXT    <- cbind(LCfit$Dxt/LCfit$Ext,LCfor$rates)

source("c:/HWANG/HJPROJECT/FUNCTION/mortality2.R")

QT <- BASEKR2$Dxt/BASEKR2$Ext
QM <- BASEKRM2$Dxt/BASEKRM2$Ext
QF <- BASEKRF2$Dxt/BASEKRF2$Ext
PT <- 1-QT
PM <- 1-QM
PF <- 1-QF

LCfit2  <- StMoMo::fit(LC, data = BASEKR2, ages.fit = ages.fit)
LCfitm2  <- StMoMo::fit(LC, data = BASEKRM2, ages.fit = ages.fit)
#LLfitm <-  llmodel(BASEKRM,BASEKR)
LCfitf2  <- StMoMo::fit(LC, data = BASEKRF2, ages.fit = ages.fit)
#LLfitf <-  llmodel(BASEKRF,BASEKR)

LCfor2  <- forecast::forecast(LCfit2, h = 50)
LCform2 <- forecast::forecast(LCfitm2, h = 50)
LCforf2 <- forecast::forecast(LCfitf2, h = 50)
MXT     <- cbind(LCfitm2$Dxt/LCfitm2$Ext,LCform2$rates)
FXT     <- cbind(LCfitf2$Dxt/LCfitf2$Ext,LCforf2$rates)
TXT     <- cbind(LCfit2$Dxt/LCfit2$Ext,LCfor2$rates)


RMXT     <- cbind(LCfitm$Dxt/LCfitm$Ext,LCform$rates)
RFXT     <- cbind(LCfitf$Dxt/LCfitf$Ext,LCforf$rates)
RTXT     <- cbind(LCfit$Dxt/LCfit$Ext,LCfor$rates)

# 남녀 기대여명
NT <- TXT %>%ncol
PT <- 1-TXT
ET <- sapply(c(1:NT),function(i){
      cumprod(PT[,i])%>%sum + (1-cumprod(PT[,i])%>%.[100])
      })
names(ET) <- c(1970:(1970+NT-1))

PM <- 1-MXT
NM <- MXT %>%ncol
  EM <- sapply(c(1:NM),function(i){
  cumprod(PM[,i])%>%sum + (1-cumprod(PM[,i])%>%.[100])
})
names(EM) <- c(1970:(1970+NM-1))

PF <- 1-FXT
NF <- FXT %>%ncol
EF <- sapply(c(1:NF),function(i){
  cumprod(PF[,i])%>%sum + (1-cumprod(PF[,i])%>%.[100])
})
names(EF) <- c(1970:(1970+NF-1))

#ultimate bx

wt <- ((EF-80)/22)%>%.[50]

# range period
# rotated lee carter

mxf <- BASEKRM$Dxt/BASEKRM$Ext
mxf <- BASEKRF$Dxt/BASEKRF$Ext
lc <- lileecarter.estimate(mxf, mxf)
rotlc <- rotate.leecarter(lc$bx, lc$ultimate.bx,ET, e0l = 80, e0u = 102, p = 0.5)

plot(lc$bx, type="l")
lines(lc$ultimate.bx, col="red")
for(i in 1:ncol(rotlc)) lines(rotlc[,i], col="grey")


# 궁극의 연령대에 도달하는 시기는 아직 없음 2009년 80

#wt
w <- sapply(c(1:NT), function(i){
  (ET[i]-80)/(102-80)}) 


t <- as.character(c(1970:2071))
#t <- as.character(c(2009:(2071)))
#t <- which(w==w["2008"])
bu <- sapply(c(1:102), function(i){
  if(i<40){LCfit$bx}
  else{LCfit$bx*(1-w[t[i]])+w[t[i]]*lc$ultimate.bx}
}
)%>%as.data.frame()
colnames(bu) <- c((1970):(2071))

# EM <- sapply(c(1:50),function(i){
#   cumprod(PM[,i])%>%sum + (1-cumprod(PM[,i])%>%.[100])
# })
# names(EM)  <- c((1970+NT-50):(1970+NT-1))

t <- as.character(c(1970:2021))
#t <- as.character(c((1970+NT-50):(1970+NT-1)))
K_LLG_M <- sapply(c(1:52),function(i) {
  my_function <- function(K) {
    A <-matrix(data = (LCfitm$ax), nrow = 100, ncol = 1)
    PM <- 1- exp(rotlc[,t[i]]%>%as.matrix()*(K) + A)
    #K=30
    RES <- cumprod(PM)%>%sum + (1-cumprod(PM)%>%.[100]) - EM[t[i]]
    return(RES)  # Example: Find the sqare root of 4
  }
  uniroot(my_function,c(-400,110), tol = 1e-15)$root
})
K_LLG_M
LCform$kt.f$mean

  K_LLG_F <- sapply(c(1:52),function(i) {
  my_function <- function(K) {
    A <-matrix(data = (LCfitf$ax), nrow = 100, ncol = 1)
    PF <- 1 - exp(rotlc[,t[i]]%>%as.matrix()*(K) + A)
    #K=30
    RES <- cumprod(PF)%>%sum + (1-cumprod(PF)%>%.[100]) - EF[t[i]]
    return(RES)  # Example: Find the sqare root of 4
  }
  uniroot(my_function,c(-400,110), tol = 1e-15)$root
})

  # 사망률 비교
  # real

  
   #과거데이터로 적합성검증 LSTM모형 추가가
  LSTMKAPPA <- readxl::read_excel("c:/work/LSTMKAPPA.xlsx",sheet="Sheet1")
  
  tmp  <- cbind(K_LLG_M,K_LLG_F)
  kappa_fvm <- cbind(STD_DT=c(2012:2021),
                    auto.arima(K_LLG_M[1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
                    auto.arima(LCfitm$k[1,1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
                    auto.arima(LCfitm$k[1,33:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
                    LCfitm$k[1,43:52]%>%as.numeric
                    )%>%as.data.frame
  colnames(kappa_fvm) <- c("STD_DT","Li-LEE-MALE","LC-MALE","변화점이후(남)","실제값")
  kappa_fvm <-kappa_fvm %>% left_join(LSTMKAPPA%>%dplyr::select(-LSTMF),by="STD_DT")
  
  kappa_fvf <- cbind(STD_DT=c(2012:2021),
                    auto.arima(K_LLG_F[1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
                    auto.arima(LCfitf$k[1,1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
                    auto.arima(LCfitf$k[1,33:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
                    LCfitf$k[1,43:52]%>%as.numeric
  )%>%as.data.frame
  colnames(kappa_fvf) <- c("STD_DT","Li-LEE-FEMALE","LC-FEMALE","변화점이후(여)","실제값")
  kappa_fvf <-kappa_fvf %>% left_join(LSTMKAPPA%>%dplyr::select(-LSTMM),by="STD_DT")
  
  
   tmp  <- cbind(K_LLG_M,K_LLG_F)
   kappa_fv <- cbind(STD_DT=c(2012:2021),
      auto.arima(K_LLG_M[1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
      auto.arima(K_LLG_F[1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
      auto.arima(LCfitm$k[1,1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
      auto.arima(LCfitf$k[1,1:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
      auto.arima(LCfitm$k[1,33:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric,
      auto.arima(LCfitf$k[1,33:42]%>%as.numeric)%>%forecast(h = 10)%>%.$mean%>%as.numeric
      )%>%as.data.frame
   colnames(kappa_fv) <- c("STD_DT","Li-LEE-MALE","Li-LEE-FEMALE","LC-MALE","LC-FEMALE","변화점이후(남)","변화점이후(여)")
   kappa_fv <-kappa_fv %>% left_join(LSTMKAPPA,by="STD_DT")
   tmp5 <- cbind(LCfit2$ax,LCfitm2$ax,LCfitf2$ax)
   tmp6 <- cbind(LCfit2$bx,LCfitm2$bx,LCfitf2$bx)
    
   #REAL
   mxm <- BASEKRM$Dxt/BASEKRM$Ext
   mxf <- BASEKRF$Dxt/BASEKRF$Ext
   mxm_llg   <- exp(LCfitm$bx%>%as.matrix()%*%kappa_fv$`Li-LEE-MALE`+matrix(data = (LCfitm$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxf_llg   <- exp(LCfitf$bx%>%as.matrix()%*%kappa_fv$`Li-LEE-FEMALE`+matrix(data = (LCfitf$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxm_lc    <- exp(LCfitm$bx%>%as.matrix()%*%kappa_fv$`LC-MALE`+matrix(data = (LCfitm$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxf_lc    <- exp(LCfitf$bx%>%as.matrix()%*%kappa_fv$`LC-FEMALE`+matrix(data = (LCfitf$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxm_lstm  <- exp(LCfitm$bx%>%as.matrix()%*%kappa_fv$LSTMM+matrix(data = (LCfitm$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxf_lstm  <- exp(LCfitf$bx%>%as.matrix()%*%kappa_fv$LSTMF+matrix(data = (LCfitf$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxm_abm   <- exp(LCfitm$bx%>%as.matrix()%*%kappa_fv$`변화점이후(남)`+matrix(data = (LCfitm$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   mxf_abf   <- exp(LCfitf$bx%>%as.matrix()%*%kappa_fv$`변화점이후(여)`+matrix(data = (LCfitf$ax)%>%as.matrix(), nrow = 100, ncol = 10))%>%round(5)
   #MSE,RMSE
   
   mxm <- mxm%>%.[c(50:100),]
   mxf <- mxf%>%.[c(50:100),]
   mxm_llg   <-    mxm_llg%>%.[c(50:100),]
   mxf_llg   <-    mxf_llg%>%.[c(50:100),]
   mxm_lc    <-    mxm_lc %>%.[c(50:100),]
   mxf_lc    <-    mxf_lc %>%.[c(50:100),]
   mxm_lstm  <-  mxm_lstm%>%.[c(50:100),]
   mxf_lstm  <-  mxf_lstm%>%.[c(50:100),]
   
   MSEM <- data.frame(LC     =sum((mxm_lc  -mxm[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      LLG    =sum((mxm_llg -mxm[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      LSTM   =sum((mxm_lstm-mxm[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      AB   =sum((mxm_abm-mxm[,c(2012:2021)%>%as.character()])^2)/(10*100)
   )%>%round(4)
   
   
   MSEF <- data.frame(LC     =sum((mxf_lc  -mxf[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      LLG    =sum((mxf_llg -mxf[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      LSTM   =sum((mxf_lstm-mxf[,c(2012:2021)%>%as.character()])^2)/(10*100),
                      AB   =sum((mxf_abf-mxf[,c(2012:2021)%>%as.character()])^2)/(10*100)
   )%>%round(4)
   
   RMSEM <- data.frame(LC     =(sum((mxm_lc  -mxm[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       LLG    =(sum((mxm_llg -mxm[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       LSTM   =(sum((mxm_lstm-mxm[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       AB   =(sum((mxm_abm-mxm[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 
   )%>%round(4)
   
   RMSEF <- data.frame(LC     =(sum((mxf_lc  -mxf[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       LLG    =(sum((mxf_llg -mxf[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       LSTM   =(sum((mxf_lstm-mxf[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 ,
                       AB   =(sum((mxf_abf[,]-mxf[,c(2012:2021)%>%as.character()])^2)/(10*100))^0.5 
   )%>%round(4)
   
   
   MAPEM <- data.frame(LC     =sum(((mxm_lc[-c(1:50),]  -mxm[-c(1:50),c(2012:2021)%>%as.character()])/mxm[-c(1:50),c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       LLG    =sum(((mxm_llg[-c(1:50),] -mxm[-c(1:50),c(2012:2021)%>%as.character()])/mxm[-c(1:50),c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       LSTM   =sum(((mxm_lstm[-c(1:50),]-mxm[-c(1:50),c(2012:2021)%>%as.character()])/mxm[-c(1:50),c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       AB   =sum(((mxm_abm[-c(1:50),]-mxm[-c(1:50),c(2012:2021)%>%as.character()])/mxm[-c(1:50),c(2012:2021)%>%as.character()])%>%abs)/1000*100
   )%>%round(4)
  
   
   MAPEF <- data.frame(LC     =sum(((mxf_lc[-c(1:50),]  -mxf[-c(1:50),c(2012:2021)%>%as.character()])/mxf[-c(1:50),c(2012:2021)%>%as.character()])%>%abs)/1000*100 ,
                       LLG    =sum(((mxf_llg[-c(1:50),] -mxf[-c(1:50),c(2012:2021)%>%as.character()])/mxf[-c(1:50),c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       LSTM   =sum(((mxf_lstm[-c(1:50),]-mxf[-c(1:50),c(2012:2021)%>%as.character()])/mxf[-c(1:50),c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       AB   =sum(((mxf_abf[-c(1:50),]-mxf[-c(1:50),c(2012:2021)%>%as.character()])/mxf[-c(1:50),c(2012:2021)%>%as.character()])%>%abs)/1000*100
   )%>%round(4)
   


   
   RMSEM <- data.frame(LC     =sum(((mxm_lc[]  -mxm [,c(2012:2021)%>%as.character()])/mxm[,c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       LLG    =sum(((mxm_llg [] -mxm[,c(2012:2021)%>%as.character()])/mxm[,c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       LSTM   =sum(((mxm_lstm[]-mxm [,c(2012:2021)%>%as.character()])/mxm[,c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       AB   =sum(((mxm_abm   []-mxm [,c(2012:2021)%>%as.character()])/mxm[,c(2012:2021)%>%as.character()])%>%abs)/1000*100
   )%>%round(4)
   
   
   RMSEF <- data.frame(LC     =sum(((mxf_lc  []  -mxf[,c(2012:2021)%>%as.character()])/mxf[,c(2012:2021)%>%as.character()])%>%abs)/1000*100 ,
                       LLG    =sum(((mxf_llg [] -mxf [,c(2012:2021)%>%as.character()])/mxf[,c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       LSTM   =sum(((mxf_lstm[]-mxf  [,c(2012:2021)%>%as.character()])/mxf[,c(2012:2021)%>%as.character()])%>%abs)/1000*100,
                       AB   =sum(((   mxf_abf[]-mxf  [,c(2012:2021)%>%as.character()])/mxf[,c(2012:2021)%>%as.character()])%>%abs)/1000*100
   )%>%round(4)
   
   
   # 
   # xlsx::write.xlsx(tmp ,"c:/work/kappa.xlsx", sheetName="tmp",append=F)
   # xlsx::write.xlsx(tmp1 ,"c:/work/kappa.xlsx", sheetName="tmp1",append=T)
   # xlsx::write.xlsx(tmp2 ,"c:/work/kappa.xlsx", sheetName="tmp2",append=T)
   # xlsx::write.xlsx(tmp3 ,"c:/work/kappa.xlsx", sheetName="tmp3",append=T)
   # xlsx::write.xlsx(tmp4 ,"c:/work/kappa.xlsx", sheetName="tmp4",append=T)  
   # xlsx::write.xlsx(tmp5 ,"c:/work/kappa.xlsx", sheetName="tmp5",append=T)
   # xlsx::write.xlsx(tmp6,"c:/work/kappa.xlsx",  sheetName="tmp6",append=T)

   