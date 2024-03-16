# llmodel
# data2 <- BASEKR
# data <- BASEKRM
# dataf <- BASEKRF$Dxt/BASEKRF$Ext


llmodel <- function(data,data2){
  
  data <- data$Dxt/data$Ext
  # q
  # t<-0
  b<- array(1:1, dim=c(nrow(data),1))
  bm<- array(1:1, dim=c(nrow(data),1))
  bf<- array(1:1, dim=c(nrow(data),1))
  # y<- seq(from=1 , to=1, length=81)
  a<- array(0:0, dim=c(nrow(data),ncol(data)))
  am<- array(0:0, dim=c(nrow(data),ncol(data)))
  #
  af<- array(0:0, dim=c(nrow(data),ncol(data)))
  #
  # z<- array(0:0, dim=c(80,28))
  e<- array(0:0,dim=c(nrow(data),ncol(data)))
  k<- array(0:0, dim=c(1,ncol(data)))
  kf<- array(0:0, dim=c(1,ncol(data)))
  km<- array(0:0, dim=c(1,ncol(data)))
  # ck<- array(0:0, dim=c(1,28))
  pre<- array(0:0, dim=c(1,ncol(data)))
  res <- list()
  ages.fit <- 0:99
  wxt <- genWeightMat(ages = ages.fit, years = data2$years, clip = 3)
  # mean log mortality
  LCfitt  <- StMoMo::fit(LC, data = data2, ages.fit = ages.fit, wxt = wxt)
  
  
  for( i in 1 : nrow(data)){
    a[i,] <- log(mean(as.numeric(data[i,1:ncol(data)])))
  }
  
  z<- log(data[,c(1:ncol(data))]) -  a
  
  
  
  
  z <-z - (LCfitt$bx%*% LCfitt$kt)
  
  
  
  a.svd<-svd(z)
  u<-a.svd$u
  v<-a.svd$v
  d<-a.svd$d
  
  
  l=max(d)
  for(x in 1:nrow(data)){
    
    for(t in 1:ncol(data))	{
      e[x,t]=l*u[x,1]*v[t,1]
    }
  }
  
  for(t in 1:ncol(data)){
    for(x in 1:nrow(data))
    {
      k[1,t]=e[x,t]+k[1,t]
    }
  }
  
  for(x in 1:nrow(data)){
    b[x,1]=e[x,2]/k[1] 
  }
  
  ll <- a + (LCfitt$bx%*% LCfitt$kt)  + b%*%k
   res$b <- b
  res$k <- k
  res$B <- LCfitt$bx
  res$K <- LCfitt$kt
  
  
  return(res)
}


QM <- BASEKR$Dxt/BASEKR$Ext
QF <- BASEKRF$Dxt/BASEKRF$Ext
PM <- 1-QM
PF <- 1-QF
cumprod(P[,1])%>%sum + (1-cumprod(P[,1])%>%.[100])
sapply(c(1:50),function(i){
  cumprod(P[,i])%>%sum + (1-cumprod(P[,i])%>%.[100])
})



ui <-     navbarPage("LI-LEE류", theme = shinytheme("flatly"),
                     tabPanel("기대여명",dashboardPage(dashboardHeader(),
                                                   dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2020-01-01') , end = as.Date('2023-12-31'))),
                                                   dashboardBody(fluidRow(box(plotOutput("EM"), width =6, solidHeader = TRUE),box(plotOutput("EF"), width =6, solidHeader = TRUE)),
                                                                 fluidRow(box(plotOutput("EE"), width =12, solidHeader = TRUE))
                                                   ))))
server <- function(input, output)
{   
  output$EM <- renderPlot({
    QM <- MXT
    PM <- 1-QM
    EM <- sapply(c(1:102),function(i){
      cumprod(PM[,i])%>%sum + (1-cumprod(PM[,i])%>%.[100])
    })
    data.frame(STD_DT=c(1970:2071),EM,80,102)%>%cplot
    
  })
  output$EF <- renderPlot({
    QF <- FXT
    PF <- 1-QF
    EF <- sapply(c(1:102),function(i){
      cumprod(PF[,i])%>%sum + (1-cumprod(PF[,i])%>%.[100])
    })
   
    data.frame(STD_DT=c(1970:2071),EF,80,102)%>%cplot
  })
  

}

shinyApp(ui, server)
wt <- ((EF-80)/22)%>%.[50]

rep(LCfitf$bx%>%.[16:65]%>%mean,65)

LCform <- forecast(LCfitm, h = 50)
LCforf <- forecast(LCfitf, h = 50)
MXT<- cbind(LCfitm$Dxt/LCfitm$Ext,LCform$rates)
FXT<- cbind(LCfitf$Dxt/LCfitf$Ext,LCforf$rates)

MXT[1,102]/
(1-(1-MXT[c(16:20),102])%>%prod)
bu<- c(rep(LCfitf$bx%>%.[16:65]%>%mean,65),LCfitf$bx%>%.[66:100])
bxt<- c((1-wt)*LCfitf$bx)+(wt)*bu
#bxt<- c(rep(LCfitf$bx%>%.[16:65]%>%mean,65),bxt%>%.[66:100])
data.frame(STD_DT=c(0:99),bxt,LCfitf$bx,bu)%>%cplot

