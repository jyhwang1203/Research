ipak<- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

pkg <-c("demography","reshape2","dplyr","reshape2","plyr","MortCast","MortalityLaws","vars",
        "nloptr","tseries","urca",'zoo','StMoMo',"TSA","ggplot2","xlsx","gridExtra")
ipak(pkg)
###한국(종합)



df <- list()
lifetable   <- read.csv("c:/work/lifetable2.csv",stringsAsFactors = FALSE,header=T,fileEncoding="EUC-KR") 

lifetable[,1] <- gsub('.{1}$', '', lifetable[,1])
lifetable <- lifetable[lifetable[,1]!="100세이",]
colnames(lifetable)[-c(1,2)] <- lifetable%>%colnames%>%.[-c(1,2)] %>% substr(2,5)
ncol(lifetable)
#lifetable<-lifetable[,-c((ncol(lifetable)-9):ncol(lifetable))]
 #lifetable<-lifetable[,-c((ncol(lifetable)-5):ncol(lifetable))]
 #lifetable <-lifetable[,-c(3:12)]

lifetable$index[lifetable$index=="사망확률(전체)"] <- "mor_t"
lifetable$index[lifetable$index=="사망확률(남자)"] <- "mor_m"
lifetable$index[lifetable$index=="사망확률(여자)"] <- "mor_f"
lifetable$index[lifetable$index=="정지인구(전체)"] <- "exp_t"
lifetable$index[lifetable$index=="정지인구(남자)"] <- "exp_m"
lifetable$index[lifetable$index=="정지인구(여자)"] <- "exp_f"
lifetable$index[lifetable$index=="사망자(전체)"] <- "dea_t"
lifetable$index[lifetable$index=="사망자(남자)"] <- "dea_m"
lifetable$index[lifetable$index=="사망자(여자)"] <- "dea_f"

lifetable %>% filter(index=="mor_m")%>%.[,-c(1,2)] 

df <- list()
df$dead  <- lifetable %>% filter(index=="dea_t")%>%.[,-c(1,2)] %>%as.matrix
df$exp   <- lifetable %>% filter(index=="exp_t")%>%.[,-c(1,2)] %>%as.matrix
df_mortality  <- lifetable %>% filter(index=="mor_t")%>%.[,-c(1,2)] %>%as.matrix
df$Year <-  lifetable%>%colnames%>%.[-c(1,2)] %>% as.numeric()
df$Age  <-  c(0:99)%>%as.numeric()

YEAR=unique(df$Year);nC=length(YEAR)
AGE =unique(df$Age);nL=length(AGE)

MUH =matrix(df$dead/df$exp,nL,nC)
POPH=matrix(df$exp,nL,nC)
BASEH <- demogdata(data=MUH, pop=POPH, ages=AGE, years=YEAR, type="mortality",
                   label="korea", name="total", lambda=0)
BASEKR <- StMoMoData(BASEH, series = "total") 

###한국(남성)

df_m<- list()
df_m$dead  <- lifetable %>% filter(index=="dea_m")%>%.[,-c(1,2)] %>%as.matrix
df_m$exp   <- lifetable %>% filter(index=="exp_m")%>%.[,-c(1,2)] %>%as.matrix
df_m_mortality  <- lifetable %>% filter(index=="mor_m")%>%.[,-c(1,2)] %>%as.matrix
df_m$Year <-  lifetable%>%colnames%>%.[-c(1,2)] %>% as.numeric()
df_m$Age  <-  c(0:99)%>%as.numeric()

YEAR=unique(df_m$Year);nC=length(YEAR)
AGE =unique(df_m$Age);nL=length(AGE)

MUH =matrix(df_m$dead/df_m$exp,nL,nC)
POPH=matrix(df_m$exp,nL,nC)
BASEH <- demogdata(data=MUH, pop=POPH, ages=AGE, years=YEAR, type="mortality",
                   label="korea", name="male", lambda=0)
BASEKRM <- StMoMoData(BASEH, series = "male") 

###################################

###한국(여성)

df_f <- list()
df_f$dead  <- lifetable %>% filter(index=="dea_f")%>%.[,-c(1,2)] %>%as.matrix
df_f$exp   <- lifetable %>% filter(index=="exp_f")%>%.[,-c(1,2)] %>%as.matrix
df_f_mortality  <- lifetable %>% filter(index=="mor_f")%>%.[,-c(1,2)] %>%as.matrix
df_f$Year <-  lifetable%>%colnames%>%.[-c(1,2)] %>% as.numeric()
df_f$Age  <-   c(0:99)%>%as.numeric()

YEAR=unique(df_f$Year);nC=length(YEAR)
AGE =unique(df_f$Age);nL=length(AGE)

MUH =matrix(df_f$dead/df_f$exp,nL,nC)
POPH=matrix(df_f$exp,nL,nC)
BASEH <- demogdata(data=MUH, pop=POPH, ages=AGE, years=YEAR, type="mortality",
                   label="korea", name="female", lambda=0)
BASEKRF <- StMoMoData(BASEH, series = "female") 

# LC <- lc(link = "logit")
# LCfit <-  StMoMo::fit(LC, data = BASEKR)
# LCfitm <-  StMoMo::fit(LC, data = BASEKRM)
# LCfitf <-  StMoMo::fit(LC, data = BASEKRF)
(BASEKRF$Dxt/BASEKRF$Ext)%>%.[45,2]
(BASEKR$Dxt/BASEKRF$Ext)%>%.[45,2]
