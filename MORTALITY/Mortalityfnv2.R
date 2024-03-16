

ages.fit <- 0:99
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/llmodel.r")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/mortality.r")
LC <- lc(link = "logit")
#wxt <- genWeightMat(ages = ages.fit, years = EWMaleIniData$years, clip = 3)
RH <- rh(link = "logit", cohortAgeFun = "1")
APC <- apc(link = "logit")
CBD <- cbd()
M7 <- m7()

wxt <- genWeightMat(ages = ages.fit, years = BASEKR$years, clip = 3)


# data <- BASEKR
# datam <- BASEKRM
# dataf <- BASEKRF
LCfit  <- StMoMo::fit(LC, data = BASEKR, ages.fit = ages.fit)
APCfit <- StMoMo::fit(APC, data = BASEKR, ages.fit = ages.fit, wxt = wxt)
CBDfit <- StMoMo::fit(CBD, data = BASEKR, ages.fit = ages.fit, wxt = wxt)
#RHfit <-  StMoMo::fit(RH, data = BASEKR, ages.fit = ages.fit, wxt = wxt,start.ax = LCfit$ax, start.bx = LCfit$bx, start.kt = LCfit$kt)


LCfitm  <- StMoMo::fit(LC, data = BASEKRM, ages.fit = ages.fit)
APCfitm <- StMoMo::fit(APC, data = BASEKRM, ages.fit = ages.fit, wxt = wxt)
CBDfitm <- StMoMo::fit(CBD, data = BASEKRM, ages.fit = ages.fit, wxt = wxt)
#RHfitm <-  StMoMo::fit(RH, data = BASEKRM, ages.fit = ages.fit, wxt = wxt,start.ax = LCfitm$ax, start.bx = LCfitm$bx, start.kt = LCfitm$kt)
LLfitm <-  llmodel(BASEKRM,BASEKR)

LCfitf  <- StMoMo::fit(LC, data = BASEKRF, ages.fit = ages.fit)
CBDfitf  <- StMoMo::fit(CBD, data = BASEKRF, ages.fit = ages.fit, wxt = wxt)
APCfitf <- StMoMo::fit(APC, data = BASEKRF, ages.fit = ages.fit, wxt = wxt)
#CBDfitf <- StMoMo::fit(CBD, data = BASEKRF, ages.fit = ages.fit, wxt = wxt)
#RHfitf <-  StMoMo::fit(RH, data = BASEKRF, ages.fit = ages.fit, wxt = wxt,start.ax = LCfitf$ax, start.bx = LCfitf$bx, start.kt = LCfitf$kt)
LLfitf <-  llmodel(BASEKRF,BASEKR)


source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/mortality2.r")
wxt <- genWeightMat(ages = ages.fit, years = BASEKR$years, clip = 3)
wxt2 <- genWeightMat(ages = ages.fit, years = BASEKRF$years, clip = 3)
# LCfit2  <- StMoMo::fit(LC, data = BASEKR, ages.fit = ages.fit, wxt = wxt)
# APCfit2 <- StMoMo::fit(APC, data = BASEKR, ages.fit = ages.fit, wxt = wxt)
# CBDfit2 <- StMoMo::fit(CBD, data = BASEKR, ages.fit = ages.fit, wxt = wxt)
# RHfit2 <-  StMoMo::fit(RH, data = BASEKR, ages.fit = ages.fit, wxt = wxt,start.ax = LCfit2$ax, start.bx = LCfit2$bx, start.kt = LCfit2$kt)


LCfitm2  <- StMoMo::fit(LC, data = BASEKRM, ages.fit = ages.fit, wxt = wxt)
APCfitm2 <- StMoMo::fit(APC, data = BASEKRM, ages.fit = ages.fit, wxt = wxt)
CBDfitm2 <- StMoMo::fit(CBD, data = BASEKRM, ages.fit = ages.fit, wxt = wxt)
#RHfitm2 <-  StMoMo::fit(RH, data = BASEKRM, ages.fit = ages.fit, wxt = wxt,start.ax = LCfitm2$ax, start.bx = LCfitm2$bx, start.kt = LCfitm2$kt)
LLfitm2 <-  llmodel(BASEKRM,BASEKR)

LCfitf2  <- StMoMo::fit(LC, data = BASEKRF, ages.fit = ages.fit, wxt = wxt2)
APCfitf2 <- StMoMo::fit(APC, data = BASEKRF, ages.fit = ages.fit, wxt = wxt2)
CBDfitf2 <- StMoMo::fit(CBD, data = BASEKRF, ages.fit = ages.fit, wxt = wxt2)
#RHfitf2 <-  StMoMo::fit(RH, data = BASEKRF, ages.fit = ages.fit, wxt = wxt,start.ax = LCfitf2$ax, start.bx = LCfitf2$bx, start.kt = LCfitf2$kt)
LLfitf2 <-  llmodel(BASEKRF,BASEKRF)


LCform <- forecast(LCfitm2, h = 15)
#RHform <- forecast(RHfitm2, h = 15, gc.order = c(1, 1, 0))
CBDform <- forecast(CBDfitm2, h = 15)
APCform <- forecast(APCfitm2, h = 15, gc.order = c(1, 1, 0))


LCforf <- forecast(LCfitf2, h = 15)
#RHforf <- forecast(RHfitf2,   h = 15, gc.order = c(1, 1, 0))
CBDforf <- forecast(CBDfitf2, h = 15)
APCforf <- forecast(APCfitf2, h = 15, gc.order = c(1, 1, 0))

tt <- auto.arima(LLfitm2$k%>%as.numeric)%>%forecast(h = 15)
ttt <- auto.arima(LLfitm2$K%>%as.numeric)%>%forecast(h = 15)
llm <- LLfitm2$a + (LLfitm2$B %*% LLfitm2$K)  + LLfitm2$b%*%LLfitm2$k
ratem <- exp(cbind(llm,(LLfitm2$a[,c(1:15)] + LLfitm2$b%*%tt$mean+ LLfitm2$B %*% ttt$mean)))

tt    <- auto.arima(LLfitf2$k%>%as.numeric)%>%forecast(h = 15)
ttt   <- auto.arima(LLfitf2$K%>%as.numeric)%>%forecast(h = 15)
llf   <- LLfitf2$a + (LLfitf2$B %*% LLfitf2$K)  + LLfitf2$b%*%LLfitf2$k
ratef <- exp(cbind(llf,(LLfitf2$a[,c(1:15)] + LLfitf2$b%*%tt$mean+ LLfitf2$B %*% ttt$mean)))
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/mortality.r")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/LSTMV2.r")

# AGE <- 25
# 
#       MORM25 <- data.frame(STD_DT=c("1970":"2021"),LC=FOR_MALE$res2[,2]) %>% left_join(
#       data.frame(STD_DT=c("1970":"2021"),LSTM=FOR_MALE$res2[,5]),by="STD_DT")%>%left_join(
#       data.frame(STD_DT=c("1970":"2021"),REAL=(BASEKRM$Dxt/BASEKRM$Ext)%>%.[AGE,]),by="STD_DT")%>% left_join(
#       # data.frame(STD_DT=c("2007":"2021"),APC=APCform$rates[AGE,]),by="STD_DT") %>% left_join(
#       # data.frame(STD_DT=c("2007":"2021"),CBD=CBDform$rates[AGE,]),by="STD_DT")%>% left_join(
#       data.frame(STD_DT=c("1970":"2021"),LL=ratem%>%.[AGE,]),by="STD_DT") %>% cplot
#   
#       MORF25 <-data.frame(STD_DT=FOR_FEMALE$res2[,1],LC=FOR_FEMALE$res2[,2]) %>% left_join(
#       data.frame(STD_DT=FOR_FEMALE$res2[,1],LSTM=FOR_FEMALE$res2[,5]),by="STD_DT")%>%left_join(
#       data.frame(STD_DT=c("1970":"2021"),REAL=(BASEKRF$Dxt/BASEKRF$Ext)%>%.[AGE,]),by="STD_DT")%>% left_join(
#       # data.frame(STD_DT=c("2007":"2021"),APC=APCforf$rates[AGE,]),by="STD_DT") %>% left_join(
#       # data.frame(STD_DT=c("2007":"2021"),CBD=CBDforf$rates[AGE,]),by="STD_DT")%>% left_join(
#       data.frame(STD_DT=c("1970":"2023"),LL=ratef%>%.[AGE,]),by="STD_DT") %>% cplot


AGE <- 45
      
      MORM45 <- data.frame(STD_DT=c("1970":"2021"),LC=FOR_MALE$res3[,2]) %>% left_join(
      data.frame(STD_DT=c("1970":"2021"),LSTM=FOR_MALE$res3[,5]),by="STD_DT")%>%left_join(
      data.frame(STD_DT=c("1970":"2021"),REAL=(BASEKRM$Dxt/BASEKRM$Ext)%>%.[AGE,]),by="STD_DT")%>% left_join(
      # data.frame(STD_DT=c("2007":"2021"),APC=APCform$rates[AGE,]),by="STD_DT") %>% left_join(
      # data.frame(STD_DT=c("2007":"2021"),CBD=CBDform$rates[AGE,]),by="STD_DT")%>% left_join(
      data.frame(STD_DT=c("1970":"2021"),LL=ratem%>%.[AGE,]),by="STD_DT") %>% cplot
      
      MORF45 <- data.frame(STD_DT=FOR_FEMALE$res2[,1],LC=FOR_FEMALE$res3[,2]) %>% left_join(
      data.frame(STD_DT=FOR_FEMALE$res2[,1],LSTM=FOR_FEMALE$res3[,5]),by="STD_DT")%>%left_join(
      data.frame(STD_DT=c("1970":"2021"),REAL=(BASEKRF$Dxt/BASEKRF$Ext)%>%.[AGE,]),by="STD_DT")%>% left_join(
      # data.frame(STD_DT=c("2007":"2021"),APC=APCforf$rates[AGE,]),by="STD_DT") %>% left_join(
      # data.frame(STD_DT=c("2007":"2021"),CBD=CBDforf$rates[AGE,]),by="STD_DT")%>% left_join(
      data.frame(STD_DT=c("1970":"2023"),LL=ratef%>%.[AGE,]),by="STD_DT") %>% cplot
      
      
      
AGE <- 65
      
      MORM65 <- data.frame(STD_DT=c("1970":"2021"),LC=FOR_MALE$res4[,2]) %>% left_join(
      data.frame(STD_DT=c("1970":"2021"),LSTM=FOR_MALE$res4[,5]),by="STD_DT")%>%left_join(
      data.frame(STD_DT=c("1970":"2021"),REAL=(BASEKRM$Dxt/BASEKRM$Ext)%>%.[AGE,]),by="STD_DT")%>% left_join(
      # data.frame(STD_DT=c("2007":"2021"),APC=APCform$rates[AGE,]),by="STD_DT") %>% left_join(
      # data.frame(STD_DT=c("2007":"2021"),CBD=CBDform$rates[AGE,]),by="STD_DT")%>% left_join(
      data.frame(STD_DT=c("1970":"2021"),LL=ratem%>%.[AGE,]),by="STD_DT")  %>% cplot
      
      MORF65 <- data.frame(STD_DT=FOR_FEMALE$res2[,1],LC=FOR_FEMALE$res4[,2]) %>% left_join(
      data.frame(STD_DT=FOR_FEMALE$res2[,1],LSTM=FOR_FEMALE$res4[,5]),by="STD_DT")%>%left_join(
      data.frame(STD_DT=c("1970":"2021"),REAL=(BASEKRF$Dxt/BASEKRF$Ext)%>%.[AGE,]),by="STD_DT")%>% left_join(
      # data.frame(STD_DT=c("2007":"2021"),APC=APCforf$rates[AGE,]),by="STD_DT") %>% left_join(
      # data.frame(STD_DT=c("2007":"2021"),CBD=CBDforf$rates[AGE,]),by="STD_DT")%>% left_join(
      data.frame(STD_DT=c("1970":"2023"),LL=ratef%>%.[AGE,]),by="STD_DT")  %>% cplot


      
      nnn <- 37
      nn<-FOR_TOTAL$res2 %>% nrow
      
      pfind<- function(data){
        res2 <- data
        nn   <- res2 %>% nrow
        #mape
        tmp1 <-(sum((res2%>%.[,2]-res2%>%.[,4])/res2%>%.[,4])/nn)%>%round(6)%>%abs #arima
        tmp2 <-(sum((res2%>%.[,5]-res2%>%.[,4])/res2%>%.[,4])/nn)%>%round(6)%>%abs #lstm
        #mse
        tmp3<-(sum((res2%>%.[,2] -  res2%>%.[,4])^2)/nn*100)%>%round(6)
        tmp4<-(sum((res2%>%.[,5] -  res2%>%.[,4])^2)/nn*100)%>%round(6)
        #rmse
        tmp5<-((sum((res2%>%.[,2] - res2%>%.[,4])^2)/nn*100)^0.5)%>%round(6)
        tmp6<-((sum((res2%>%.[,5] - res2%>%.[,4])^2)/nn*100)^0.5)%>%round(6)
        
        
        pfm  <-  
          cbind(
            rbind(tmp1,tmp2),
            rbind(tmp3,tmp4),
            rbind(tmp5,tmp6))
        colnames(pfm) <- c("MAPE","MSE","RMSE")
        rownames(pfm) <- c("arima","lstm")
        
        return(pfm)
      }

####ax,bx,kt#########################################################################################

lcax <- data.frame(STD_DT=c(0:99),TOTAL=LCfit$ax,
                  MALE=LCfitm$ax,
                  FEMALE=LCfitf$ax)%>%round(4)
colnames(lcax)[-1] <-  c("전체","남성","여성")

lcbx <-data.frame(STD_DT=c(0:99),TOTAL=LCfit$bx,
                  MALE=LCfitm$bx,
                  FEMALE=LCfitf$bx)%>%round(4)
colnames(lcbx)[-1] <-  c("전체","남성","여성")
lckt <-data.frame(STD_DT=c(1970:2021),TOTAL=LCfit$kt%>%t,
                  MALE=LCfitm$kt%>%t,
                  FEMALE=LCfitf$kt%>%t)%>%round(4)
colnames(lckt)[-1] <-  c("전체","남성","여성")
lckt$FEMALE
lckt$MALE
# ##########################################################################
# res1 <-  lstm(LCfit$kt,data,10,"LC",300,100,35)
# res2 <-  lstm(lckt$MALE ,BASEKR,10,"LC",300,100,35)
# res3 <-  lstm(lckt$FEMALE,BASEKR,10,"LC",300,100,35)
# 
# res4 <-  lstm(LCfit$kt,data,10,"LC",300,100,45)
# res5 <-  lstm(lckt$MALE ,BASEKR,10,"LC",300,100,45)
# res6 <-  lstm(lckt$FEMALE,BASEKR,10,"LC",300,100,45)
#   
# ####mortality#########################################################################################
# FOR1 <- forecast(auto.arima(lckt$TOTAL))
# FOR2 <- forecast(auto.arima(lckt$MALE))
# FOR3 <- forecast(auto.arima(lckt$FEMALE))
# 
# TMP2 <- 
#   cbind(
#     exp((LCfit$ax+LCfit$bx%*%FOR1$mean))%>%.[46,],
#     exp((LCfit$ax+LCfit$bx%*%FOR1$lower[,-1]))%>%.[46,],
#     exp((LCfit$ax+LCfit$bx%*%FOR1$upper[,-1]))%>%.[46,],
#     
#     exp((LCfit$ax+LCfit$bx%*%FOR1$mean))%>%.[45,],
#     exp((LCfit$ax+LCfit$bx%*%FOR1$lower[,-1]))%>%.[45,],
#     exp((LCfit$ax+LCfit$bx%*%FOR1$upper[,-1]))%>%.[45,],
#     
#     exp((LCfit$ax+LCfit$bx%*%FOR1$mean))%>%.[45,],
#     exp((LCfit$ax+LCfit$bx%*%FOR1$lower[,-1]))%>%.[45,],
#     exp((LCfit$ax+LCfit$bx%*%FOR1$upper[,-1]))%>%.[45,])
# 
# 
# TMP3  <- data.frame(FOR1$mean,FOR2$mean,FOR3$mean)


# data2 <- LCfit2$kt%>%t%>%ts(frequency = 1,start=c(1933),end=c(2019))
# data2 <- LCfit3$kt%>%t%>%ts(frequency = 1,start=c(1947),end=c(2019))
#arima(0,2,2)
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

data1 <- LCfit$kt%>%t%>%ts(frequency = 1,start=c(1970),end=c(2021))
data2 <- LCfitm$kt%>%t%>%ts(frequency = 1,start=c(1970),end=c(2021))
data3 <- LCfitf$kt%>%t%>%ts(frequency = 1,start=c(1970),end=c(2021))
cusum(data1)
cusum(data2)
cusum(data3)


par(mfrow=c(1,3))
plot(c(1970:2021),cusum(data1) ,type='l')
plot(c(1970:2021),cusum(data2) ,type='l')
plot(c(1970:2021),cusum(data3) ,type='l')
