#install.packages("shinydashboard")
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("ggplot2","roll","quantmod","PerformanceAnalytics","reshape","imager","png","grid","BVAR",
        "data.table","n")
ipak(pkg)
  

handling <- function(data){
  STD_DT            <-  data[-c(1:9),1]%>%as.matrix()%>%as.numeric%>%as.Date(origin = "1899-12-30")
  RAWDATA           <-  cbind(STD_DT,data[-c(1:9),-1]) %>%as.data.frame(stringsasfactors = T)%>% as.data.table()
  TEMP              <-  apply(RAWDATA%>%dplyr::select(-STD_DT), 2, as.numeric)%>%fillf
  RAWDATA2           <-  data.frame(STD_DT,TEMP)%>%mutate(STD_DT=as.Date(STD_DT))
  #%>%melt(id.vars="STD_DT")
  return(RAWDATA2)
}



dt_trans<- function(data){
  
  data <- data.table(STD_DT=index(data),data)
    
  return(data)
}

cplot <- function(data,name){
  data%>%reshape2::melt(id.vars = "STD_DT")%>%na.omit%>%
    ggplot(aes(x=STD_DT, y=value, col = variable,fill=variable)) +             
    geom_line(size=1)+ggtitle(as.character(name))
}

trans_rt <- function(data,st){

  data <- data%>%data.frame()
  {if(st=="day"){
    data <- as.xts(data[,-1],order.by = data$STD_DT )%>% Return.calculate(method = c("discrete", "log"))%>%na.omit
  }
    else if(st=="week"){
      data <- as.xts(data[,-1],order.by = data$STD_DT )%>% Return.calculate(method = c("discrete", "log")) %>% apply.weekly(., Return.cumulative)%>%na.omit
    }
    else if(st=="month"){
      data <- as.xts(data[,-1],order.by = data$STD_DT )%>% Return.calculate(method = c("discrete", "log")) %>% apply.monthly(., Return.cumulative)%>%na.omit
    }
    else if(st=="quarter"){
      data <- as.xts(data[,-1],order.by = data$STD_DT )%>% Return.calculate(method = c("log")) %>% apply.quarterly(., Return.cumulative)%>%na.omit
    }
    else if(st=="year"){
      data <- as.xts(data[,-1],order.by = data$STD_DT )%>% Return.calculate(method = c("discrete", "log")) %>% apply.yearly(., Return.cumulative)%>%na.omit
    }
  }
  return(data)
}

trans_rt2 <- function(data){
  data <- data%>%data.frame()
  data <- as.xts(data[,-1],order.by = (data$STD_DT) )
  return(data)
}



fillf <-  function(data){    
  for(c in 1:ncol(data)){
    for(r in 2:nrow(data)){
      if(data[r,c] %>% is.na)
        data[r,c] <- data[r-1,c] 
      
    }  
  }
  return(data)    
}
tb_per <-  function(data){
  tmp <- 
    rbind(
      data%>%select(-STD_DT)%>%apply(2,mean)*12,
      data%>%select(-STD_DT)%>%apply(2,sd)*12^0.5,
      maxDrawdown(data%>%select(-STD_DT))%>%round(4)
    )%>%round(3)
  rownames(tmp)<- c("MEAN","VOL","MDD")
  return(tmp)
}





cuml <-  function(data){
  
#  return(data.table(STD_DT=data$STD_DT,100*((1+data%>%dplyr::select(-STD_DT))%>%cumprod)-1)-100 )
  return(data.table(STD_DT=data$STD_DT,((1+data%>%dplyr::select(-STD_DT))%>%cumprod)-1))
}

     graphbar <- function(data,col,title){
       RES <- data%>% melt(id.vars="STD_DT") %>%ggplot(aes(x=reorder(variable,-value), y=value,fill=col)) +  
         geom_bar(position="dodge", stat="identity", fill=paste0(col)) + theme(axis.text.x = element_text(angle = 90, hjust = 1,size=20))+ 
         ggtitle(paste("'",title,"'")) 
       
       return(RES)
     }
     
     graphbar2 <- function(data,title){

       # 
       # RES <- RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%filter(STD_DT<"1990-01-01")%>%mutate(BM=0.6*SP500+0.4*USGOVT)%>%
       #   mutate(USGOVT=USGOVT)%>%mutate(SP500=SP500)%>%trans_rt("year")%>%dt_trans%>%melt(id.vars="STD_DT")  %>%   ggplot( aes(x=STD_DT, y=value, fill=variable)) +
       #   geom_bar(stat="identity", position=position_dodge())
       # 
       
       
       RES <- data%>% melt(id.vars="STD_DT")  %>%   ggplot(aes(x=STD_DT, y=value,fill=variable)) +  
         geom_bar(stat="identity",position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1,size=20))+ 
         ggtitle(paste("'",title,"'")) 
       
       return(RES)
     }
     
     png("filename.png")
     plot(1:10)
     dev.off()
     
     
     exname <- function(data){
       
       data%>%reshape::rename(c("WORLD"="글로벌주식",
                                "DM"="선진국주식",
                                "EM"="신흥국주식",
                       "DOW"="다우지수",
                          "SP500"="S&P500",
                          "NASDAQ"="나스닥",
                          "KOSPI"="코스피",
                          "EURO50"="유로스톡스",
                          "FTSE"="FTSE",
                          "NIKKEI"="니케이",
                          "DAX"="독일DAX",
                          "CAC"="프랑스CAC",
                          "TSX"="캐나나TSX",
                          "BVSP"="보베스파",
                          "SHANGHAI"="상해지수",
                          "CSI300"="CSI300",
                          "HANGS"="항셍",
                           "NIFTY"="니프티",
                           "USSHORT"="미국단기국채",
                           "WRBOND"="글로벌채권",
                           "WRGOVT"="글로벌국채",
                           "WRIG"="글로벌IG",
                           "WRHY"="글로벌HY",
                           "USGOVT"="미국국채",
                           "USBOND"="미국채권",
                           "USIG"="미국IG",
                           "USHY"="미국HY",
                           "EUBOND"="유럽채권",
                           "EUIG"="유럽IG",
                           "EUHY"="유럽HY",
                           "EUGOVT"="유럽국채",
                           "KRBOND"="한국채권",
                           "EMBOND"="신흥국채권",
                           "CNBOND"="중국채권",
                           "USLONG"="미국장기국채",
                           "USMID"="미국중기국채",
                           "DXY"="달러",
                           "WORLD"="글로벌채권",
                           "MUN"="미국지방채",
                           "KRBONDH"="국내채권(USD)",
                                                 "AL"="대체투자",
                       "CRBTR"="원자재",
                       "EMGOVT"="신흥국국채(USD)",
                       "EMGOVTL"="신흥국국채(로컬)",
                       "FEPS_WORLD"   ="세계",
                       "FEPS_DM"   ="선진국",
                       "FEPS_EM"   ="신흥국",
                       "FEPS_DOW"     ="다우지수",
                       "FEPS_SP500"   ="S&P500",
                       "FEPS_NASDAQ"  ="나스닥",
                       "FEPS_KOSPI"   ="코스피",
                       "FEPS_EURO50"  ="유로스톡스",
                       "FEPS_FTSE"    ="FTSE",
                       "FEPS_NIKKEI"  ="니케이",
                       "FEPS_DAX"     ="독일DAX",
                       "FEPS_CAC"     ="프랑스CAC",
                       "FEPS_TSX"     ="캐나나TSX",
                       "FEPS_BVSP"    ="보베스파",
                       "FEPS_SHANGHAI"="상해지수",
                       "FEPS_CSI300"  ="CSI300",
                       "FEPS_HANGS"   ="항셍",
                       "FEPS_NIFTY"   ="니프티",
                      "DGS30"="US30Y",
                      "DGS10"="US10Y",
                      "DGS2"="US2Y",
                      "BAMLC0A0CM"="IGSP",
                      "BAMLH0A0HYM2"="HYSP",
                      "VIXCLS"="VIX",
                      "DTWEXBGS"="DXY",
                      "DEXKOUS"="USDKRW",
                      "DFII10"="TIPS",
                      "FEDFUNDS"="USBR",
                      "macro"="매크로",
                      "trend"="추세추종",
                      "rlvalue"="Relative Value",
                      "hem"="EM",
                      "hed"="EVENT-DRIVEN",
                      "hcr"="CREDIT",
                      "eqh"="EQUITY-HEDGE",
                      "hfri"="HFRI종합",
                      "USGROWTH"="미국성장",
                      "USVALUE"="미국가치",
                      "USLVOL"="미국저변동성",
                      "USHDIV"="미국고배당",
                      "USMOM"="미국모멘텀",
                      "USQUAL"="미국퀄리티",
                      "KRGROWTH"="한국성장",
                      "KRVALUE"="한국가치",
                      "FEPS_USGROWTH"="미국성장",
                      "FEPS_USVALUE"="미국가치",
                      "FEPS_USLVOL"="미국저변동성",
                      "FEPS_USHDIV"="미국고배당",
                      "FEPS_USMOM"="미국모멘텀",
                      "FEPS_USQUAL"="미국퀄리티",
                      "FEPS_KRGROWTH"="한국성장",
                      "FEPS_KRVALUE"="한국가치",
                      "PEF"="사모펀드",
                      "USREIT"="미국리츠",
                      "EUCREIT"="유럽코어리츠",
                      "ASIACREIT"="아시아코어리츠",
                      "USCREIT"="미국코어리츠",
                      "WRCINFRA"="글로벌코어인프라",
                      "GSCI"="GSCI원자재",
                      "CRBTR"="CRB원자재",
                      "WRINFRA"="글로벌인프라",
                      "WREPRA"="글로벌부동산",
                      "HFRI"="헤지펀드",
                      "HMACRO"="해지펀드(매크로)",
                      "GOLD"="금",
                      "WTI"="원유",
                      "FNG1"="미국천연가스",
                      "ENGM1"="유럽천연가스",
                      "BITC"="비트코인",
                      "MSKR"="한국주식",
                      "SIL"="은",
                      "COP"="구리",
                      "ISMPMIM"="ISM제조업지수",
                      "ISMPMIS"="ISM비제조업지수",
                      "USCPIYOY"="미국CPI(YoY)",
                      "USCORECPIYOY"="미국CoreCPI(YoY)",
                      "USBR"="미국기준금리",
                      "KRBR"="한국기준금리",
                      "CNBR"="중국기준금리",
                      "EUBR"="유로기준금리",
                      "JPBR"="일본기준금리",
                      "UKBR"="영국기준금리",
                      "AUBR"="호주기준금리",
                      "BRBR"="브라질기준금리",
                      "INBR"="인도기준금리",
                      "UKCPI"="영국CPI(YoY)",
                      "MSKRT"="한국주식(Total)",
                      "WORLDT"="해외주식(Total)"
                           ))
     }
     
     
     train_sec <- function(primary, secondary, na.rm = TRUE) {
       # Thanks Henry Holm for including the na.rm argument!
       from <- range(secondary, na.rm = na.rm)
       to   <- range(primary, na.rm = na.rm)
       # Forward transform for the data
       forward <- function(x) {
         rescale(x, from = from, to = to)
       }
       # Reverse transform for the secondary axis
       reverse <- function(x) {
         rescale(x, from = to, to = from)
       }
       list(fwd = forward, rev = reverse)
     }
     
       ftmacro <- function(STD_DT1,ASSET){
     
      
       # lamda <- 0.99
       TMP <- retm %>% filter(STD_DT<STD_DT1&STD_DT>STD_DT1-years(10))
       # <- "MSUS"
       TMP2 <- paste("lm(",ASSET,"~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%summary")
       res <- eval(parse(text=TMP2))
       TMP <- res$coefficients[,1]
       
       return(TMP)
       }
       
     
       
       rollcor <- function(asset1,asset2,range1,n,nn){
       tmp  <- RAWDATA%>%select(STD_DT,asset1,asset2) %>% trans_rt("month")%>%dt_trans%>%na.omit%>%filter(STD_DT >= range1 & STD_DT <=(range1%>%as.Date()+years(nn))) 
       tmp  <- data.table(STD_DT=tmp$STD_DT,ROLLIMG5YEAR = roll_cor(tmp[,2]%>%as.matrix(), tmp[,3]%>%as.matrix(), width = n))%>%na.omit
       colnames(tmp)[2] <- paste(asset1,asset2)
       tmp
       }
       
       
       PA  <- function(data){
       
         TMP    <- data.frame(STD_DT=data$STD_DT,cumprod(1+data[,-1]))
         mean   <- (TMP[,-1]%>%tail(n=1))^(12/data%>%nrow) -1
         sig    <- ((data[,-1])%>%apply(2,var) * 12)^0.5
         mdd    <-  maxDrawdown(data%>%trans_rt2())
         sr     <-  (mean-0.02)/sig
         sk   <-  data[,-1]%>%skewness%>%round(4)
         kt   <-  data[,-1]%>%kurtosis%>%round(4)
        
         srratio<- sapply(c(1:(ncol(data)-1)),function(i){
         
            (mean[i]-0.02)/data[data[,i+1]<0 ,i+1]%>%sd
           
         })
         tmp <- rbind(mean,sig,mdd,sr,srratio,sk,kt)%>%round(4)
         rownames(tmp)<-c("MEAN","VOL","MDD","SR","SORTINO","SKEWNESS","KURTOSIS")
         
         return(tmp)
       }

       
       