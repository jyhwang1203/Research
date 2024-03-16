#3XBM
  RAWDATA %>% filter(variable=="NASDAQ"|variable=="USBOND")%>%dcast(STD_DT~variable)%>%na.omit%>%trans_rt("month")%>%dt_trans%>%
  mutate(TQQQ=3*NASDAQ)%>%mutate(TMF=3*USBOND)%>%dplyr::select(STD_DT,TQQQ,TMF)%>%
  mutate(BM1=0.6*TQQQ+0.4*TMF)%>%
  mutate(BM2=0.5*TQQQ+0.5*TMF)%>%
  mutate(BM3=0.4*TQQQ+0.6*TMF)%>%
  cuml

  RAWDATA %>% filter(variable=="NASDAQ"|variable=="USBOND")%>%dcast(STD_DT~variable)%>%na.omit%>%trans_rt("day")%>%dt_trans%>%
    mutate(TQQQ=3*NASDAQ)%>%mutate(TMF=3*USBOND)%>%dplyr::select(STD_DT,TQQQ,TMF)%>%
    mutate(BM1=0.6*TQQQ+0.4*TMF)%>%
    mutate(BM2=0.5*TQQQ+0.5*TMF)%>%
    mutate(BM3=0.4*TQQQ+0.6*TMF)%>%
    apply.yearly(., Return.cumulative)
  
#BM
  TMP <- RAWDATA %>% filter(variable=="WORLDT"|variable=="WRBOND"|variable=="MSUST")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans%>%
  filter(STD_DT>"2000-01-01")%>%
  mutate(BM1=0.7*WORLDT+0.3*WRBOND)%>%
  mutate(BM2=0.7*0.7*MSUST+0.7*0.3*WORLDT+0.3*WRBOND)%>%
  mutate(BM3=0.7*0.6*MSUST+0.7*0.4*WORLDT+0.3*WRBOND)%>%dplyr::select(STD_DT,BM1,BM2,BM3)%>%
  cuml
  TMP%>%cplot
  
  TMP2 <-RAWDATA %>% filter(variable=="WORLDT"|variable=="WRBOND")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans%>%
    filter(STD_DT>"2000-01-01")%>%
    mutate(BM1=0.7*WORLDT+0.3*WRBOND)%>%
    mutate(BM2=0.5*WORLDT+0.5*WRBOND)%>%
    mutate(BM3=0.3*WORLDT+0.7*WRBOND)%>%
    PA
    
write.xlsx(TMP ,"c:/work/monthly.xlsx", sheetName="TMP",append=F) 
write.xlsx(TMP ,"c:/work/monthly.xlsx", sheetName="TMP",append=F) 

#####
getSymbols('ACWI', src='yahoo');getSymbols('SPY', src='yahoo');getSymbols('QQQ', src='yahoo') ;getSymbols('EWY', src='yahoo')
getSymbols('VGK', src='yahoo') ;getSymbols('EWJ', src='yahoo');getSymbols('ASHR', src='yahoo');getSymbols('EMXC', src='yahoo')           
getSymbols('IUSB', src='yahoo');getSymbols('FLOT', src='yahoo');getSymbols('EMB', src='yahoo');getSymbols('VCSH', src='yahoo')
getSymbols('BKLN', src='yahoo')

getSymbols('AGG', src='yahoo')
getSymbols('GOVT', src='yahoo')
getSymbols('USIG', src='yahoo')
getSymbols('LQD', src='yahoo')
getSymbols('EMB', src='yahoo')
getSymbols('HYG', src='yahoo')

getSymbols('IFRA', src='yahoo')
getSymbols('IUSB', src='yahoo')
getSymbols('BKLN', src='yahoo')
getSymbols('HYG', src='yahoo')
getSymbols('AGG', src='yahoo')
getSymbols('EMB', src='yahoo')

getSymbols('PAVE', src='yahoo')
getSymbols('REET', src='yahoo')
getSymbols('VNQ' , src='yahoo')



#벤치마크
BM <- RAWDATA%>%filter(variable=="WRINFRA"|variable=="WORLD"|variable=="SP500"|variable=="WRBOND")%>%dcast(STD_DT~variable)%>%na.omit%>%trans_rt("month")%>%dt_trans
RAWDATA%>%filter(variable=="WRINFRA"|variable=="WORLD"|variable=="WREPRA"|variable=="SP500")%>%dcast(STD_DT~variable)%>%na.omit%>%trans_rt("month")%>%
  dt_trans%>%cuml%>%cplot

#회사채
USIG$USIG.Adjusted %>%dt_trans()%>%left_join(
  VCSH$VCSH.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
    LQD$LQD.Adjusted %>%dt_trans(),by="STD_DT")%>%
     na.omit%>%trans_rt("month")%>%dt_trans%>%
      filter(STD_DT>"2000-10-01")%>%cuml%>%cplot

#종합채권
AGG$AGG.Adjusted %>%dt_trans()%>%left_join(
  IUSB$IUSB.Adjusted %>%dt_trans(),by="STD_DT")%>%
   na.omit%>%trans_rt("month")%>%dt_trans%>%
    filter(STD_DT>"2000-10-01")%>%cuml%>%cplot

#하이일드
BKLN$BKLN.Adjusted %>%dt_trans()%>%left_join(
  HYG$HYG.Adjusted %>%dt_trans(),by="STD_DT")%>%
   na.omit%>%trans_rt("month")%>%dt_trans%>%
    filter(STD_DT>"2000-10-01")%>%cuml%>%cplot

AGG$AGG.Adjusted %>%dt_trans()%>%left_join(
  LQD$LQD.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
    EMB$EMB.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
      HYG$HYG.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
      RAWDATA%>%filter(variable=="WRBOND")%>%dcast(STD_DT~variable),by="STD_DT")%>%
  na.omit%>%trans_rt("month")%>%dt_trans%>%
  filter(STD_DT>"2000-10-01")%>%cuml%>%cplot

#주식
EWY$EWY.Adjusted %>%dt_trans()%>%left_join(
  ACWI$ACWI.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
  SPY$SPY.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
    QQQ$QQQ.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
      VGK$VGK.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
        EWJ$EWJ.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
          ASHR$ASHR.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
            EMXC$EMXC.Adjusted %>%dt_trans(),by="STD_DT")%>%filter(STD_DT>"2023-11-30")%>%
  na.omit%>%trans_rt("month")%>%dt_trans

  cuml%>%cplot
  
#대체투자
  REET$REET.Adjusted %>%dt_trans()%>%left_join(
    PAVE$PAVE.Adjusted %>%dt_trans(),by="STD_DT")%>%left_join(
      VNQ$VNQ.Adjusted %>%dt_trans(),by="STD_DT")%>%
       na.omit%>%trans_rt("month")%>%dt_trans%>%
       filter(STD_DT>"2000-10-01")%>%cuml%>%cplot  
  
#KISMP(ETF 버전)  
#KISMP(INDEX버전)
  
ret <- RAWDATA %>% filter(variable=="WORLDT"|variable=="MSUST"|variable=="MSEUT"|variable=="MSJPT"|variable=="MSCNT"|variable=="EMEXCNT"|variable=="MSKRT"|
                   variable=="WRBOND"|variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|variable=="KRBOND"|
                   variable=="GSCI"|variable=="WREPRA"|variable=="WRINFRA"|variable=="MSJPT")%>%dcast(STD_DT~variable)%>%na.omit%>%trans_rt("month")%>%dt_trans%>%melt(id.vars="STD_DT")

ret <- RAWDATA %>% filter(variable=="WORLDT"|variable=="MSUST"|variable=="MSEUT"|variable=="MSJPT"|variable=="MSCNT"|variable=="EMEXCNT"|variable=="MSKRT"|
                            variable=="WRBOND"|variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|variable=="KRBOND"|
                            variable=="GSCI"|variable=="WREPRA"|variable=="WRINFRA"|variable=="MSJPT")%>%dcast(STD_DT~variable)%>%na.omit%>%trans_rt("year")%>%dt_trans%>%melt(id.vars="STD_DT")


TMP <- readxl::read_excel("c:/work/MPPORT.xlsx",sheet="KISMP1")%>%filter(STD_DT==("2024-03-31")%>%as.Date())%>%dt_trans%>%melt(id.vars="STD_DT")%>%.[,-1]%>%
  left_join(ret,by=c("variable"))%>%mutate(rt=value.x*value.y)%>%na.omit%>%dcast(STD_DT~variable,value.var = "rt")

TMP2  <-RAWDATA %>% filter(variable=="WORLDT"|variable=="WRBOND"|variable=="MSUST")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans%>%
  filter(STD_DT>"2000-01-01")%>%
  mutate(BM1=0.7*WORLDT+0.3*WRBOND)%>%dplyr::select(STD_DT,BM1)

TMP2  <-RAWDATA %>% filter(variable=="WORLDT"|variable=="WRBOND"|variable=="MSUST")%>%dcast(STD_DT~variable)%>%trans_rt("year")%>%dt_trans%>%
  filter(STD_DT>"2000-01-01")%>%
  mutate(BM1=0.7*WORLDT+0.3*WRBOND)%>%dplyr::select(STD_DT,BM1)

data.frame(STD_DT=TMP[,1],TMP[,-1]%>%apply(1,sum))%>%left_join(TMP2,by="STD_DT")
data.frame(STD_DT=TMP[,1],TMP[,-1]%>%apply(1,sum))%>%left_join(TMP2,by="STD_DT")%>%cuml%>%cplot
data.frame(STD_DT=TMP[,1],TMP[,-1]%>%apply(1,sum))%>%left_join(TMP2,by="STD_DT")%>%PA

TMP2  <-RAWDATA %>% filter(variable=="WORLDT"|variable=="WRBOND"|variable=="MSUST")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans%>%
  filter(STD_DT>"2000-01-01")%>%
  mutate(BM1=0.7*WORLDT+0.3*WRBOND)%>%dplyr::select(STD_DT,BM1)



TMP <- RAWDATA %>% filter(variable=="WRBOND"|variable=="WRIG"|variable=="WRHY"|variable=="KRBOND"|variable=="WRGOVT")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans%>%
  filter(STD_DT>"2010-01-01")%>%cuml
TMP%>%cplot
