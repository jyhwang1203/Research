  SDATE <- ("2004-01-01")%>%as.Date()
  LDATE <- ("2024-01-01")%>%as.Date()
  
  reth <- RAWDATA%>%
    filter(variable=="MSKRT"|variable=="WORLDT"|variable=="WRBOND"|
             variable=='EMBOND'|variable=='GSCI'|variable=="KRBONDH"|variable=="USBOND"|
             variable=="WREPRA"|variable=="WRINFRA")%>%
    dcast(STD_DT~variable)%>%na.omit%>%trans_rt("quarter")%>%dt_trans%>%filter(STD_DT>=LDATE&STD_DT<"2024-01-01")%>%as.data.frame
  retcum <- RAWDATA%>%
    filter(variable=="MSKRT"|variable=="WORLDT"|variable=="WRBOND"|
             variable=='EMBOND'|variable=='GSCI'|variable=="KRBONDH"|variable=="USBOND"|
             variable=="WREPRA"|variable=="WRINFRA")%>%
    dcast(STD_DT~variable)%>%na.omit%>%trans_rt("quarter")%>%dt_trans%>%filter(STD_DT<"2024-01-01")%>%as.data.frame
  retcum <- RAWDATA%>%
    filter(variable=="MSKRT"|variable=="WORLDT"|variable=="WRBOND"|variable=="USBOND"|
             variable=='EMBOND'|variable=='GSCI'|variable=="KRBONDH"|
             variable=="WREPRA"|variable=="WRINFRA")%>%
    dcast(STD_DT~variable)%>%na.omit%>%trans_rt("quarter")%>%dt_trans%>%filter(STD_DT>="2004-01-01"&STD_DT<"2024-01-01")%>%
    cuml%>%as.data.frame
  retcum%>%dplyr::select(STD_DT,KRBONDH,USBOND,WRBOND)%>%cplot("tt")
  ret <- RAWDATA%>%
    filter(variable=="MSKRT"|variable=="WORLDT"|variable=="USBOND"|
            variable=='EMBOND'|variable=='GSCI'|variable=="KRBONDH"|
             variable=="WREPRA"|variable=="WRINFRA")%>%
    dcast(STD_DT~variable)%>%na.omit%>%trans_rt("quarter")%>%dt_trans%>%filter(STD_DT<LDATE&STD_DT>=SDATE)
  
   #\ ret<- RAWDATA %>% filter(variable=="USCPIYOY"|variable=="USGDPQ")%>%na.omit%>%dcast(STD_DT~variable) %>% mutate(USGDPQ=USGDPQ/100)%>%
   #   inner_join(ret,by="STD_DT") %>%mutate(USCPIYOY=(USCPIYOY/100)%>%as.numeric())
   # 
  ret <- as.xts(ret[,-1]%>%data.frame,order.by = (ret$STD_DT)%>%as.Date )%>%data.frame
  x <- ret[,c("MSKRT","WORLDT","KRBONDH","USBOND","EMBOND","WREPRA","WRINFRA","GSCI")]
  retcum <- retcum[,c("MSKRT","WORLDT","KRBONDH","USBOND","EMBOND","WREPRA","WRINFRA","GSCI")]
  
  
  ################################################### code chunk number 5: minnesota
  mn <- bv_minnesota(lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 1e-04, max = 5), 
                     alpha = bv_alpha(mode = 2), var = 1e+07)
  ################################################### code chunk number 6: dummies
  soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
  sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)
  #################################################8 code chunk number 7: priors
  priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)
  ################################################### code chunk number 8: metropolis
  mh <- bv_metropolis(scale_hess = c(0.05, 1e-04, 1e-04), adjust_acc = TRUE, acc_lower = 0.25, 
                      acc_upper = 0.45)
  ################################################## code chunk number 9: bvar
  run <- BVAR::bvar(x , lags = 1, n_draw = 50000, n_burn = 25000, n_thin = 1, priors = priors, 
              mh = mh, verbose = TRUE)
  ################################################## code chunk number 10: print

  pred <- predict(run, horizon = 8, conf_bands = c(0.01, 0.05))
  summary(run)
  # ret
   RT_R <- reth%>%filter(STD_DT>"2018-12-31")%>%melt(id.vars = "STD_DT")
   TTMP <- lapply(c(1:ncol(ret)), function(i){
    
   index<- (pred$variables)
   tmp <-data.frame(STD_DT=(RT_R$STD_DT)%>%unique,t(pred$quants[,,i]),
              reth%>%filter(STD_DT>"2018-12-31")%>%.[,index[i]])
   colnames(tmp) <- c("STD_DT","1%","5%","50%","95%","99%",index[i])
   tmp
   data.frame(STD_DT=RT_R$STD_DT,tmp%>%t)
   })
 #TTMP[[4]]%>%cplot("ff")
   RES3 <- sapply(c(1:ncol(x)),function(t){
     (sapply(c(5:8),function(i){
  
       pred$fcast[,i,t]%>%mean(trim=0.1)
     })+1)%>%cumprod
   })
  
  RES3 <- sapply(c(1:8),function(t){
    (sapply(c(3:6),function(i){
      
      pred$fcast[,i,t]
    })+1)%>%apply(1,cumprod)%>%tail(n=1)%>%mean(trim=0.05)
  })%>%t%>%as.data.frame
  

  colnames(RES3) <- pred$variables
  RT_E <- (RES3%>%tail(n=1))
  RT_E["USBOND"]<- RT_E["USBOND"] +0.02
  RT<-rbind(RT_E,((retcum%>%tail(n=1))+1)^(4/80))
  RT
  COR_E <-(diag(vcov(run)%>%diag()%>%sqrt)%>%inv) %*% vcov(run) %*% (diag(vcov(run)%>%diag()%>%sqrt)%>%inv) 
  colnames(COR_E)<- rownames(vcov(run))
  COR_H<-  cor(x)
  VOL <- (vcov(run)%>%diag%>%sqrt)*4^0.5
  
  
  
  
  xlsx::write.xlsx(RT ,"c:/work/BVAR.xlsx", sheetName="RT",append=F)
  xlsx::write.xlsx(COR_E ,"c:/work/BVAR.xlsx", sheetName="COR_E",append=T)
  xlsx::write.xlsx(COR_H ,"c:/work/BVAR.xlsx", sheetName="COR_H",append=T)
  xlsx::write.xlsx(VOL ,"c:/work/BVAR.xlsx", sheetName="VOL",append=T)
  xlsx::write.xlsx(TTMP[[1]] ,"c:/work/BVAR.xlsx", sheetName="RES1",append=T)
  xlsx::write.xlsx(TTMP[[2]] ,"c:/work/BVAR.xlsx", sheetName="RES2",append=T)
  xlsx::write.xlsx(TTMP[[3]] ,"c:/work/BVAR.xlsx", sheetName="RES3",append=T)
  xlsx::write.xlsx(TTMP[[4]] ,"c:/work/BVAR.xlsx", sheetName="RES4",append=T)
  xlsx::write.xlsx(TTMP[[5]] ,"c:/work/BVAR.xlsx", sheetName="RES5",append=T)
  xlsx::write.xlsx(TTMP[[6]] ,"c:/work/BVAR.xlsx", sheetName="RES6",append=T)
  xlsx::write.xlsx(TTMP[[7]] ,"c:/work/BVAR.xlsx", sheetName="RES7",append=T)
  xlsx::write.xlsx(TTMP[[7]] ,"c:/work/BVAR.xlsx", sheetName="RES8",append=T)
  xlsx::write.xlsx(retcum ,"c:/work/BVAR.xlsx", sheetName="rcum",append=T)
  
  RT_R <- reth%>%filter(STD_DT>"2018-12-31")%>%melt(id.vars = "STD_DT")
  RT_E <- data.frame(STD_DT=RT_R$STD_DT, RES)%>%melt(id.vars = "STD_DT")
  RT_L <- data.frame(STD_DT=RT_R$STD_DT, RES2)%>%melt(id.vars = "STD_DT")
  RT_U <- data.frame(STD_DT=RT_R$STD_DT, RES3)%>%melt(id.vars = "STD_DT")
  
  RT_R%>%left_join(RT_E,by=c("STD_DT","variable")) %>% filter(variable=="WORLD")%>%.[,-2]%>%cplot("dd")
  RT_R%>%left_join(RT_E,by=c("STD_DT","variable")) %>% filter(variable=="WRBOND")%>%.[,-2]%>%cplot("dd")
  RT_R%>%left_join(RT_E,by=c("STD_DT","variable"))%>%
    left_join(RT_L,by=c("STD_DT","variable"))%>%
    left_join(RT_U,by=c("STD_DT","variable"))%>% filter(variable=="EMBOND")%>%.[,-2]%>%cplot("dd")
  
  
  RES3 <- sapply(c(1:7),function(t){
    (sapply(c(1:20),function(i){
      
      pred$fcast[,i,t] %>% quantile(prob=0.99)
    })+1)
  })-1
  colnames(RES3) <- pred$variables
  
  rt_l1  <- (RES%>%tail(n=1))^0.2
  rt1 <- (RES%>%tail(n=1))^0.2
  rt_l1
  rt_l2
  vcov(run)
  coef(run)
  vcov(run)%>%diag()%>%sqrt%>%det
  cor <-(diag(vcov(run)%>%diag()%>%sqrt)%>%inv) %*% vcov(run) %*% (diag(vcov(run)%>%diag()%>%sqrt)%>%inv)
  colnames(cor)<- rownames(vcov(run))
  cor(ret)
    ret <- RAWDATA%>%
    filter(variable=="WORLD"|variable=="MSKR"|
             variable=="WRBOND"|variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|
             variable=="WREPRA"|variable=="WRINFRA")%>%
    dcast(STD_DT~variable)%>%na.omit%>%trans_rt("quarter")%>%dt_trans%>%filter(STD_DT<LDATE&STD_DT>SDATE)
  
    TMP <- (sapply(c(1:20),function(i){
    
    pred$fcast[,4,1]
    })+1)
  apply(TMP,1,cumprod)%>%View
  TMP2 <- apply(TMP,1,cumprod)
  TMP2%>%tail(n=1)%>%mean
  TMP2%>%tail(n=1)%>%median
  (TMP2%>%tail(n=1)%>%mean)^0.2
  RES3 %>% View
  RES3 %>%na.omit apply(1,mean)
  plot(ret[,7],type="l")
  ################################################### code chunk number 1: preliminaries
  options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
  ################################################### code chunk number 2: setup 
  set.seed(42)

  
  ################################################### code chunk number 11: trace_density (eval = FALSE) plot(run) plot(run, type =
  ################################################### 'dens', vars_response = 'GDPC1', vars_impulse = 'GDPC1-lag1')
  ################################################### code chunk number 12: trace_density
  summary(run)
  plot(run, type = "dens", vars_response = "WORLDT", vars_impulse = "USBOND")
  ################################################### code chunk number 13: betas
  ################################################### code chunk number 14: fitted
  fitted(run, type = "mean")
  ################################################### code chunk number 15: residuals
  plot(residuals(run, type = "mean"), vars = c("WORLD", "WRBOND"))
  ################################################### code chunk number 16: irf
  opt_irf <- bv_irf(horizon = 10, identification = TRUE)
  irf(run) <- irf(run, opt_irf, conf_bands = c(0.05, 0.16))
  ################################################### code chunk number 17: irf_cholesky
  run$variables <- c("한국주식","해외주식","한국채권","해외채권","신흥국채권","부동산","인프라","원자재")
  plot(irf(run), area = TRUE, vars_impulse = c("해외주식"),vars_response = c("한국채권","해외채권","원자재"))
  plot(irf(run), area = TRUE, vars_impulse = c("해외주식"),vars_response = c("한국주식","부동산","인프라"))
  
  plot(irf(run), area = TRUE, vars_impulse = c("해외주식"),vars_response = c("해외채권"))
  plot(irf(run), area = TRUE, vars_impulse = c("해외주식"),vars_response = c("한국주식"))
  
  
  ################################################### code chunk number 18: predict

  pred$fcast[,,1]
  pred$fcast[,,15]
  run$fcast
  vcov(run)
  coef(run)
  logLik(run)#2485
 
  
    xlsx::write.xlsx(RES ,"c:/work/BVAR.xlsx", sheetName="RT",append=F)
    xlsx::write.xlsx(RT_H ,"c:/work/BVAR.xlsx", sheetName="RT_H",append=T)
  ############################### code chunk number 19: predict_unconditiona
  plot(predict(run), area = TRUE, t_back = 20,vars = c("WORLD", "MSKR","MSUS","MSEU","MSJP","MSCN","EMEXCN"))
  plot(predict(run), area = TRUE, t_back = 20)
  plot(predict(run), area = TRUE, t_back = 20,vars = c("해외주식(Total)"))

  ################################################### code chunk number 20: app_data
  y <- fred_qd[1:243, c("GDPC1", "GDPCTPI", "FEDFUNDS")]
  z <- fred_transform(y, type = "fred_qd")
  y <- fred_transform(y, codes = c(5, 5, 1), lag = 4)
  
  
  ################################################### code chunk number 21: app_timeseries
  op <- par(mfrow = c(1, 3), mar = c(3, 3, 1, 0.5), mgp = c(2, 0.6, 0))
  plot(as.Date(rownames(y)), y[, "GDPC1"], type = "l", xlab = "Time", ylab = "GDP growth")
  plot(as.Date(rownames(y)), y[, "GDPCTPI"], type = "l", xlab = "Time", ylab = "Inflation")
  plot(as.Date(rownames(y)), y[, "FEDFUNDS"], type = "l", xlab = "Time", ylab = "Federal funds rate")
  par(op)
  
  
  ################################################### code chunk number 22: app_bvar
  priors_app <- bv_priors(mn = bv_mn(b = 0))
  run_app <- bvar(y, lags = 5, n_draw = 50000, n_burn = 25000, priors = priors_app, 
                  mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE), verbose = FALSE)
  
  
  ################################################### code chunk number 23: app_dummies
  add_soc <- function(Y, lags, par) {
    soc <- if (lags == 1) {
      diag(Y[1, ])/par
    } else {
      diag(colMeans(Y[1:lags, ]))/par
    }
    X_soc <- cbind(rep(0, ncol(Y)), matrix(rep(soc, lags), nrow = ncol(Y)))
    return(list(Y = soc, X = X_soc))
  }
  
  
  ################################################### code chunk number 24: app_priors
  soc <- bv_dummy(mode = 1, sd = 1, min = 1e-04, max = 50, fun = add_soc)
  priors_soc <- bv_priors(soc = soc)
  
  
  ################################################### code chunk number 25: app_coda
  library("coda")
  run_mcmc <- as.mcmc(run_app, vars = "lambda")
  geweke.diag(run_mcmc)
  
  
  ################################################### code chunk number 26: app_parallel
  library("parallel")
  n_cores <- 4
  cl <- makeCluster(n_cores)
  
  runs <- par_bvar(cl = cl, data = y, lags = 5, n_draw = 50000, n_burn = 25000, n_thin = 1, 
                   priors = priors_app, mh = bv_mh(scale_hess = 0.5, adjust_acc = TRUE))
  stopCluster(cl)
  
  runs_mcmc <- as.mcmc(runs, vars = "lambda")
  gelman.diag(runs_mcmc, autoburnin = FALSE)
  
  
  ################################################### code chunk number 27: app_chains
  plot(runs, type = "full", vars = "lambda")
  
  
  ################################################### code chunk number 28: app_signs
  sr <- matrix(c(1, 1, 1, -1, 1, NA, -1, -1, 1), ncol = 3)
  opt_signs <- bv_irf(horizon = 16, fevd = TRUE, identification = TRUE, sign_restr = sr)
  print(opt_signs)
  
  
  ################################################### code chunk number 29: app_irf
  irf(run_app) <- irf(run_app, opt_signs)
  
  
  ################################################### code chunk number 30: app_irf_signs
  plot(irf(run_app), vars_impulse = c(1, 3))
  
  
  ################################################### code chunk number 31: app_predict
  path <- c(2.25, 3, 4, 5.5, 6.75, 4.25, 2.75, 2, 2, 2)
  predict(run_app) <- predict(run_app, horizon = 16, cond_path = path, cond_var = "FEDFUNDS")
  
  
  ################################################### code chunk number 32: app_predict_conditional
  plot(predict(run_app), t_back = 16)
  ################################################### code chunk number 4: timeseries
  par(mfrow = c(4, 3))
  
  plot(x$STD_DT, x$WORLDT, type = "l", xlab = "Time", ylab = "ACWI")
  plot(x$STD_DT, x$MSUST , type = "l", xlab = "Time", ylab = "USA")
  plot(x$STD_DT, x$MSEUT , type = "l", xlab = "Time", ylab = "EURO")
  plot(x$STD_DT, x$MSJPT , type = "l", xlab = "Time", ylab = "JAPAN")
  plot(x$STD_DT, x$MSCNT , type = "l", xlab = "Time", ylab = "CHINA")
  plot(x$STD_DT, x$MSKRT , type = "l", xlab = "Time", ylab = "KOREA")
  plot(x$STD_DT, x$EMEXCNT, type = "l", xlab = "Time", ylab = "EM(EXCHINA)")
  plot(x$STD_DT, x$WRBONDT, type = "l", xlab = "Time", ylab = "GLOBAL BOND")
  plot(x$STD_DT, x$WRGOVTT, type = "l", xlab = "Time", ylab = "GLOBAL GOVERMENTBOND")
  plot(x$STD_DT, x$WRIGT , type = "l", xlab = "Time", ylab = "GLOBAL IG")
  plot(x$STD_DT, x$WRHYT , type = "l", xlab = "Time", ylab = "GLOBAL HY")
  plot(x$STD_DT, x$SPGST , type = "l", xlab = "Time", ylab = "COMMODITY")
  par(op)
  
  ui <-       navbarPage("Dashboard", theme = shinytheme("flatly"),
                         tabPanel("기준금리",dashboardPage(dashboardHeader(),
                                                       dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('1970-01-01') , end = as.Date('2023-12-31')),
                                                                        checkboxGroupInput("country", "Variables to show:",c("미국CPI"="USCPIYOY","미국CORECPI"="USCORECPIYOY","미국기준금리"="USBR","미국10년물"="US10Y","미국2년물"="US2Y",
                                                                                                                             "TIP10Y"="TIP10Y","BEI10Y"="BEI10Y"),selected="USCPIYOY")),
                                                       dashboardBody(fluidRow(box(plotOutput("cpi"), width =12, solidHeader = TRUE)),
                                                                     fluidRow(box(plotOutput("country"), width =12, solidHeader = TRUE))
                                                       ))))
  server <- function(input, output){
    output$cpi   <- renderPlot({
      RAWDATA%>%filter(variable==input$country)%>%filter(STD_DT>input$range)%>%dcast(STD_DT~variable)%>%exname%>%cplot+
        theme(legend.text = element_text(size=15))
    })
  }
  shinyApp(ui, server) 
  
  RT_H <-  RAWDATA%>%
    filter(variable=="WORLD"|variable=="MSUS"|variable=="MSEU"|variable=="MSKR"|variable=="MSJP"|variable=="MSCN"|variable=="EMEXCN"|
             variable=="WRBOND"|variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|
             variable=="WREPRA"|variable=="WRINFRA")%>%filter(STD_DT<LDATE&STD_DT>SDATE)%>%
    dcast(STD_DT~variable)%>%na.omit%>%trans_rt("year")%>%dt_trans 
  
  
  RAWDATA%>%
    filter(variable=="DM"|variable=="MSKR"|variable=="EM"|
             
             variable=="WREPRA"|variable=="WRINFRA")%>%
    dcast(STD_DT~variable)%>%na.omit%>%trans_rt("week")%>%dt_trans%>%filter(STD_DT<LDATE&STD_DT>SDATE)%>%cuml%>%cplot("dd")
  
  ret <- RAWDATA%>%
    filter(variable=="WORLD"|variable=="MSUS"|variable=="MSEU"|variable=="MSKR"|variable=="MSJP"|variable=="MSCN"|variable=="EMEXCN"|
             variable=="WRBOND"|variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|variable=='WRTIP'|variable=="DXY"|variable=="GSCI"|
             variable=="WREPRA"|variable=="WRINFRA")%>%
    dcast(STD_DT~variable)%>%na.omit%>%trans_rt("quarter")%>%dt_trans%>%filter(STD_DT<LDATE&STD_DT>SDATE)
  
  ret <- RAWDATA%>%
    filter(variable=="MSUS"|variable=="MSEU"|variable=="MSKR"|variable=="MSJP"|variable=="MSCN"|variable=="EMEXCN"|
             variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|variable=="GSCI"|
             variable=="WREPRA"|variable=="WRINFRA")%>%
    dcast(STD_DT~variable)%>%na.omit%>%trans_rt("quarter")%>%dt_trans%>%filter(STD_DT<LDATE&STD_DT>SDATE)
  