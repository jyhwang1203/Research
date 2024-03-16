
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/llmodel.r")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/mortality.r")
LC <- lc(link = "logit")
ages.fit <- 20:70
#wxt <- genWeightMat(ages = ages.fit, years = EWMaleIniData$years, clip = 3)
RH <- rh(link = "logit", cohortAgeFun = "1")
APC <- apc(link = "logit")
CBD <- cbd()
M7 <- m7()



wxt <- genWeightMat(ages = ages.fit, years = BASEKR$years, clip = 3)
BASEKRF$Dxt
BASEKR$Dxt
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
CBDfitf  <- StMoMo::fit(LC, data = BASEKRF, ages.fit = ages.fit, wxt = wxt)
APCfitf <- StMoMo::fit(APC, data = BASEKRF, ages.fit = ages.fit, wxt = wxt)
CBDfitf <- StMoMo::fit(CBD, data = BASEKRF, ages.fit = ages.fit, wxt = wxt)
#RHfitf <-  StMoMo::fit(RH, data = BASEKRF, ages.fit = ages.fit, wxt = wxt,start.ax = LCfitf$ax, start.bx = LCfitf$bx, start.kt = LCfitf$kt)
LLfitf <-  llmodel(BASEKRF,BASEKR)

# 
# source("c:/Users/ghkdw/OneDrive/바탕 화면/황지연/NaverCloud/HWANG/PAPER/mortality2.r")
# wxt <- genWeightMat(ages = ages.fit, years = BASEKR$years, clip = 3)
# 
# LCfit2  <- StMoMo::fit(LC, data = BASEKR, ages.fit = ages.fit, wxt = wxt)
# APCfit2 <- StMoMo::fit(APC, data = BASEKR, ages.fit = ages.fit, wxt = wxt)
# CBDfit2 <- StMoMo::fit(CBD, data = BASEKR, ages.fit = ages.fit, wxt = wxt)
# #RHfit2 <-  StMoMo::fit(RH, data = BASEKR, ages.fit = ages.fit, wxt = wxt,start.ax = LCfit2$ax, start.bx = LCfit2$bx, start.kt = LCfit2$kt)
# 
# 
# LCfitm2  <- StMoMo::fit(LC, data = BASEKRM, ages.fit = ages.fit, wxt = wxt)
# APCfitm2 <- StMoMo::fit(APC, data = BASEKRM, ages.fit = ages.fit, wxt = wxt)
# CBDfitm2 <- StMoMo::fit(CBD, data = BASEKRM, ages.fit = ages.fit, wxt = wxt)
# #RHfitm2 <-  StMoMo::fit(RH, data = BASEKRM, ages.fit = ages.fit, wxt = wxt,start.ax = LCfitm2$ax, start.bx = LCfitm2$bx, start.kt = LCfitm2$kt)
# LLfitm2 <-  llmodel(BASEKRM,BASEKR)
# 
# LCfitf2  <- StMoMo::fit(LC, data = BASEKRF, ages.fit = ages.fit, wxt = wxt)
# APCfitf2 <- StMoMo::fit(APC, data = BASEKRF, ages.fit = ages.fit, wxt = wxt)
# CBDfitf2 <- StMoMo::fit(CBD, data = BASEKRF, ages.fit = ages.fit, wxt = wxt)
# #RHfitf2 <-  StMoMo::fit(RH, data = BASEKRF, ages.fit = ages.fit, wxt = wxt,start.ax = LCfitf2$ax, start.bx = LCfitf2$bx, start.kt = LCfitf2$kt)
# LLfitf2 <-  llmodel(BASEKRF,BASEKR)
# 
# 
# #RHform <- forecast(RHfitm2, h = 10, gc.order = c(1, 1, 0))
# LCform <- forecast(LCfitm2, h = 10)
# CBDform <- forecast(CBDfitm2, h = 10)
# APCform <- forecast(APCfitm2, h = 10, gc.order = c(1, 1, 0))
# 
# #RHforf <- forecast(RHfitf2, h = 10, gc.order = c(1, 1, 0))
# LCforf <- forecast(LCfitf2, h = 10)
# CBDforf <- forecast(CBDfitf2, h = 10)
# APCforf <- forecast(APCfitf2, h = 10, gc.order = c(1, 1, 0))

tt <- auto.arima(LLfitm2$k%>%as.numeric)%>%forecast
ttt <- auto.arima(LLfitm2$K%>%as.numeric)%>%forecast
llm <- LLfitm2$a + (LLfitm2$B %*% LLfitm2$K)  + LLfitm2$b%*%LLfitm2$k
ratem <- exp(cbind(llm,(LLfitm2$a[,c(1:10)] + LLfitm2$b%*%tt$mean+ LLfitm2$B %*% ttt$mean)))

tt <- auto.arima(LLfitf2$k%>%as.numeric)%>%forecast
ttt <- auto.arima(LLfitf2$K%>%as.numeric)%>%forecast
llf <- LLfitf2$a + (LLfitf2$B %*% LLfitf2$K)  + LLfitf2$b%*%LLfitf2$k
ratef <- exp(cbind(llf,(LLfitf2$a[,c(1:10)] + LLfitf2$b%*%tt$mean+ LLfitf2$B %*% ttt$mean)))

# source("c:/Users/ghkdw/OneDrive/바탕 화면/황지연/NaverCloud/HWANG/PAPER/mortality.r")
# res4 <-   lstm(8,0.75,100,300,BASEKR)
# res4m <-  lstm(8,0.75,100,300,BASEKRM)
# res4f <-  lstm(8,0.75,100,300,BASEKRF)
# 
# 
# res5 <-   lstm(LCfit$kt,BASEKR,10,"LC",300,100,55)
# res5m <-  lstm(LCfitm$kt,BASEKRM,10,"LC",300,100,55)
# res5f <-  lstm(LCfitf$kt,BASEKRF,10,"LC",300,100,55)
# 
# source("c:/Users/ghkdw/OneDrive/바탕 화면/황지연/NaverCloud/HWANG/PAPER/LSTMV2.r")
# TEMPM <-cbind(
#         c((BASEKRM$Dxt/BASEKRM$Ext)%>%.[45,],APCform$rates[45,])%>%as.numeric(),
#         c((BASEKRM$Dxt/BASEKRM$Ext)%>%.[45,],CBDform$rates[45,])%>%as.numeric(),
#         ratem[45,],
#         res4m$mor)%>%data.frame
# 
# TEMPF <-cbind(
#   c((BASEKRF$Dxt/BASEKRF$Ext)%>%.[45,],RHforf$rates[45,])%>%as.numeric(),
#   c((BASEKRF$Dxt/BASEKRF$Ext)%>%.[45,],APCforf$rates[45,])%>%as.numeric(),
#   c((BASEKRF$Dxt/BASEKRF$Ext)%>%.[45,],CBDforf$rates[45,])%>%as.numeric(),
#   ratef[45,],
#   res4f$mor)%>%data.frame
# 
# 
# colnames(TEMPM)[1:4]<- c("RH","APC",'CBD',"LL")
# colnames(TEMPF)[1:4]<- c("RH","APC",'CBD',"LL")
# g1 <-TEMPM%>%select(STD_DT,LL,LSTM,REAL,ARIMA,APC,CBD,RH)%>%filter(STD_DT>1970)%>%
#   melt(id.vars="STD_DT")%>%ggplot(aes(x=STD_DT ,y= value, col = variable)) +   geom_point() +          
#   geom_line(size=1)
# 
# 
#  g2 <-TEMPF%>%select(STD_DT,LL,LSTM,REAL,ARIMA,APC,CBD,RH)%>%filter(STD_DT>1970)%>%
#   melt(id.vars="STD_DT")%>%ggplot(aes(x=STD_DT ,y= value, col = variable)) +   geom_point() +          
#   geom_line(size=1)
#  grid.arrange(g1,g2,
#               ncol = 2, nrow = 1)
 

####ax,bx,kt#########################################################################################

lcax <- data.frame(STD_DT=c(20:70),TOTAL=LCfit$ax,
                  MALE=LCfitm$ax,
                  FEMALE=LCfitf$ax)%>%round(2)
colnames(lcax)[-1] <-  c("전체","남자","여자")

lcbx <-data.frame(STD_DT=c(20:70),TOTAL=LCfit$bx,
                  MALE=LCfitm$bx,
                  FEMALE=LCfitf$bx)%>%round(4)
colnames(lcbx)[-1] <-  c("전체","남자","여자")
lckt <-data.frame(STD_DT=c(1970:2021),TOTAL=LCfit$kt%>%t,
                  MALE=LCfitm$kt%>%t,
                  FEMALE=LCfitf$kt%>%t)%>%round(2)
colnames(lckt)[-1] <-  c("전체","남자","여자")
lckt$FEMALE
lckt$MALE

lckt <-data.frame(STD_DT=c(1970:2021),TOTAL=LCfit$kt%>%t,
                  MALE=LCfitm$kt%>%t,
                  FEMALE=LCfitf$kt%>%t)%>%round(2)
colnames(lckt)[-1] <-  c("전체","남자","여자")

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

# data2 <- LCfit2$kt%>%t%>%ts(frequency = 1,start=c(1933),end=c(2019))
# data2 <- LCfit3$kt%>%t%>%ts(frequency = 1,start=c(1947),end=c(2019))
#arima(0,2,2)



source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/LSTMV2.r")
# par(mfrow=c(1,3))
# plot(c(1970:2021),cusum(data1) ,type='l')
# plot(c(1970:2021),cusum(data2) ,type='l')
# plot(c(1970:2021),cusum(data3) ,type='l')
