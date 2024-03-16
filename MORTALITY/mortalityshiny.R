source("c:/HWANG/HJPROJECT/FUNCTION/llmodel V3.r")  
ALPHA <- data.frame(STD_DT=LCfit$ages ,TOTAL= LCfit$ax%>%as.numeric,MALE=LCfitm$ax%>%as.numeric,FEMALE=LCfitf$ax%>%as.numeric)
  data1 <- LCfit$kt%>%t%>%ts(frequency = 1,start=c(1970),end=c(2021))
  data2 <- LCfitm$kt%>%t%>%ts(frequency = 1,start=c(1970),end=c(2021))
  data3 <- LCfitf$kt%>%t%>%ts(frequency = 1,start=c(1970),end=c(2021))
  logm_t <- (BASEKR$Dxt/BASEKR$Ext)%>%log
  logm_m <- (BASEKRM$Dxt/BASEKRM$Ext)%>%log
  logm_f <- (BASEKRF$Dxt/BASEKRF$Ext)%>%log
      ui <- navbarPage("LI-LEE류",theme = shinytheme("flatly"),
         tabPanel("기대여명",dashboardPage(dashboardHeader(),
          dashboardSidebar(
           dateRangeInput('range',label = '',start = as.Date('2020-01-01') , end = as.Date('2023-12-31')),
           textInput("age", "AGE")),
           dashboardBody(
             fluidRow(shinydashboard::valueBox("Lee-Carter","모수정정",color="red", width = 12)),
             fluidRow(
               box(plotOutput("LOGM"), width  =12, solidHeader = TRUE)
             ),
             fluidRow(
               box(plotOutput("ALPHA"), width =6, solidHeader = TRUE),
               box(plotOutput("BETA") , width =6, solidHeader = TRUE),
                                  ),
             fluidRow(
               box(plotOutput("TMP1"), width  =12, solidHeader = TRUE)
             ),
             fluidRow(
               box(plotOutput("KAPPA"), width =6, solidHeader = TRUE),
               box(plotOutput("CUSUM"), width =6, solidHeader = TRUE)),
             fluidRow(shinydashboard::valueBox("Lee-Carter","미래예측",color="red", width = 12)),
             fluidRow(
               box(plotOutput("KAPPAVM"), width =6, solidHeader = TRUE),
               box(plotOutput("KAPPAVF"), width =6, solidHeader = TRUE)),  
             fluidRow(
                 box(plotOutput("MORVM"), width =6, solidHeader = TRUE),
                 box(plotOutput("MORVF"), width =6, solidHeader = TRUE)),
             fluidRow(
               box(DTOutput("MSE"), width  =4, solidHeader = TRUE),
               box(DTOutput("RMSE"), width =4, solidHeader = TRUE),
               box(DTOutput("MAPE"), width =4, solidHeader = TRUE))
             
                        ))))
       
    
     server <- function(input, output){   
          output$LOGM <- renderPlot({
            LOGMM<- data.frame(STD_DT=(logm_m%>%colnames)%>%as.numeric(),t(logm_m [c(10,30,50,70),]))
            LOGMF<- data.frame(STD_DT=(logm_f%>%colnames)%>%as.numeric(),t(logm_f [c(10,30,50,70),]))
            colnames(LOGMM)[-1] <- c("10세","30세","50세","70세")
            colnames(LOGMF)[-1] <- c("10세","30세","50세","70세")
            LOGMM<-LOGMM%>% cplot(.,"남자 로그사망률")
            LOGMF<-LOGMF%>% cplot(.,"여자 로그사망률")
            grid.arrange(LOGMM,LOGMF,ncol=2)
          })
    
          output$KAPPA <- renderPlot({
          KT <- data.frame(STD_DT=LCfit$years,TOTAL=LCfit$kt%>%as.numeric,MALE=LCfitm$kt%>%as.numeric,FEMALE=LCfitf$kt%>%as.numeric)
          KT %>% cplot(.,"KAPPA")
          })
          output$ALPHA <- renderPlot({
          ALPHA <- data.frame(STD_DT=LCfit$ages ,TOTAL= LCfit$ax%>%as.numeric,MALE=LCfitm$ax%>%as.numeric,FEMALE=LCfitf$ax%>%as.numeric)
          ALPHA %>% cplot(.,"ALPHA")
          })
          output$BETA <- renderPlot({
          BETA <- data.frame(STD_DT=LCfit$ages ,TOTAL= LCfit$bx%>%as.numeric,MALE=LCfitm$bx%>%as.numeric,FEMALE=LCfitf$bx%>%as.numeric)
          BETA %>% cplot(.,"BETA")
          })
          output$TMP1 <- renderPlot({
           a<- cbind(STD_DT=c(1970:2021),
                          ARIMA=c(LCfitm$k[1,1:40]%>%as.numeric,auto.arima(LCfitm$k[1,1:40]%>%as.numeric)%>%forecast(h = 12)%>%.$mean%>%as.numeric),
                          KAPPA=  LCfitm$k[1,1:52]%>%as.numeric
            )%>%as.data.frame%>%
            cplot("Kappa 예측치(남자)")
           b<-cbind(STD_DT=c(1970:2021),
                  ARIMA=c(LCfitf$k[1,1:40]%>%as.numeric,auto.arima(LCfitf$k[1,1:40]%>%as.numeric)%>%forecast(h = 12)%>%.$mean%>%as.numeric),
                  KAPPA=  LCfitf$k[1,1:52]%>%as.numeric
            )%>%as.data.frame%>%
              cplot("Kappa 예측치(여자)") 
            
            grid.arrange(a,b,ncol=2)
            
          })
          
          output$CUSUM <- renderPlot({
          data.frame(STD_DT=c(1970:2021), TOTAL=cusum(data1),MALE=cusum(data2),FEMALE=cusum(data3)) %>% cplot(.,"CUSUM STATISTIC")
          })
          output$KAPPAF <- renderPlot({
          data.frame(STD_DT=c(1970:2071), TOTAL=c(LCfit$kt,LCfor$kt.f$mean),MALE=c(LCfitm$kt,LCform$kt.f$mean),FEMALE=c(LCfitf$kt,LCforf$kt.f$mean))%>%
              cplot(.,"KAPPAF")
          })
          output$KAPPAVM <- renderPlot({
            kappa_fv[,c("STD_DT","Li-LEE-MALE","LC-MALE","변화점이후(남)","LSTMM")]%>%cplot(.,"Kappa")
          })
          output$KAPPAVF <- renderPlot({
            kappa_fv[,c("STD_DT","Li-LEE-FEMALE","LC-FEMALE","변화점이후(여)","LSTMF")]%>%cplot(.,"Kappa")
          })
          output$MORVM <- renderPlot({
            cbind(STD_DT=c(2012:2021),REAL=mxm[input$age,c(2012:2021)%>%as.character()],LC=mxm_lc[input$age,],
                  LLG=mxm_llg[input$age,],LSTM=mxm_lstm[input$age,],AFRERBP=mxm_abm[input$age,]
                  )%>%as.data.frame%>%
              cplot(.,"사망률 예측치(2012~2021)")
          })
          output$MORVF <- renderPlot({
            cbind(STD_DT=c(2012:2021),REAL=mxf[input$age,c(2012:2021)%>%as.character()],LC=mxf_lc[input$age,],
                  LLG=mxf_llg[input$age,],LSTM=mxf_lstm[input$age,],AFRERBP=mxf_abf[input$age,]
            )%>%as.data.frame%>%
              cplot(.,"사망률 예측치(2012~2021)")
          })
          output$MSE <- renderDT({
            data.frame(rbind(MSEM,MSEF))
          })       
          output$RMSE <- renderDT({
            data.frame(rbind(RMSEM,RMSEF)) 
          })     
          output$MAPE <- renderDT({
            data.frame(rbind(MAPEM,MAPEF))
          })     
          }
  
  shinyApp(ui, server)
  
  