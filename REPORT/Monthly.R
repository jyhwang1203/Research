w


RES1 <- RAWDATA%>%filter(variable=="USLABOR"|variable=="AHEYOY"|variable=="AHEMOM"|variable=="NFPTCH")%>%dcast(STD_DT~variable)%>%
  mutate(rolling_avg60 = rollmean(NFPTCH, k=12, fill=NA, align='right'))%>%left_join(
  RAWDATA%>%filter(variable=="USBR")%>%dcast(STD_DT~variable),by="STD_DT")

RES2 <- RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%trans_rt("day")%>%dt_trans%>%
  mutate(BM =0.6*SP500+0.4*USGOVT)%>%mutate(BM2=0.5*SP500+0.5*USGOVT)%>%mutate(BM3=0.4*SP500+0.6*USGOVT)%>%apply.yearly(., Return.cumulative)%>%na.omit%>%dt_trans
RES2 <- RES1%>%dplyr::select(STD_DT,USBR)%>%na.omit%>% left_join(RES2,by="STD_DT")
RES3 <- RAWDATA%>%filter(variable=="US10Y"|variable=="US2Y"|variable=="USLABOR"|variable=="USBR")%>%dcast(STD_DT~variable)%>%mutate(SP=US10Y-US2Y)%>%mutate(Base=min(USLABOR))%>%
  dplyr::select(-c(US10Y,US2Y))
RES4 <- data.table(STD_DT=tmp$STD_DT,ROLLIMG5YEARWR =roll_cor(tmp$SP500, tmp$USGOVT, width = 60),ROLLIMG5YEARUS =roll_cor(tmp$KOSPI, tmp$KRBOND, width = 60)) %>%
  left_join(RES1%>%dplyr::select(STD_DT,USBR)%>%na.omit,by="STD_DT")
RES5 <- RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans%>%
  mutate(BM =0.6*SP500+0.4*USGOVT)%>%mutate(BM2=0.5*SP500+0.5*USGOVT)%>%mutate(BM3=0.4*SP500+0.6*USGOVT)%>%filter(STD_DT>"1990-01-01"&STD_DT<"2010-01-01")%>%PA

RES6 <- RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans%>%
  mutate(BM =0.6*SP500+0.4*USGOVT)%>%mutate(BM2=0.5*SP500+0.5*USGOVT)%>%mutate(BM3=0.4*SP500+0.6*USGOVT)%>%filter(STD_DT>"2010-01-01"&STD_DT<"2024-01-01")%>%PA
RES7 <- RAWDATA%>%filter(variable=="TIP10Y"|variable=="BEI10Y"|variable=="USBR"|variable=="US10Y"|variable=="GOLD"|variable=="DXY")%>%dcast(STD_DT~variable)%>%filter(STD_DT>"2010-01-01"&STD_DT<"2024-01-01")
RES8 <- RAWDATA%>%filter(variable=="EMX"|variable=="EMSP"|variable=="HYSP"|variable=="IGSPAA")%>%dcast(STD_DT~variable)%>%filter(STD_DT>"2010-01-01"&STD_DT<"2024-01-01")  
write.xlsx(RES1 ,"c:/work/monthly.xlsx", sheetName="RES1",append=F)
write.xlsx(RES2 ,"c:/work/monthly.xlsx", sheetName="RES2",append=T)
write.xlsx(RES3 ,"c:/work/monthly.xlsx", sheetName="RES3",append=T)
write.xlsx(RES4 ,"c:/work/monthly.xlsx", sheetName="RES4",append=T)
write.xlsx(RES5 ,"c:/work/monthly.xlsx", sheetName="RES5",append=T)
write.xlsx(RES6 ,"c:/work/monthly.xlsx", sheetName="RES6",append=T)
write.xlsx(RES7 ,"c:/work/monthly.xlsx", sheetName="RES7",append=T)
write.xlsx(RES8 ,"c:/work/monthly.xlsx", sheetName="RES8",append=T)

QTMP<- RAWDATA%>%filter(variable=="OILINV")%>%dcast(STD_DT~variable) %>%
  mutate(rolling_avg = rollmean(OILINV, k=26, fill=NA, align='right'))

RAWDATA%>%filter(variable=="OILINV")%>%dcast(STD_DT~variable) %>%
  mutate(rolling_avg = rollmean(OILINV, k=26, fill=NA, align='right'))%>%cplot

#주식, 채권 벡테스트
ui <-  navbarPage("Dashboard", theme = shinytheme("flatly"),
                  tabPanel("기준금리",
                           dashboardPage(dashboardHeader(),
                                         dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('1970-01-01') , end = as.Date('2023-12-31')),
                                                          checkboxGroupInput("labor", "Variables to show:",c("미국실업률"="USLABOR","임금인상(YoY)"="AHEYOY","임금인상(MoM)"="AHEMOM","미국기준금리리"="USBR","장단기스프레드"="SP"),selected="USLABOR"),
                                                          checkboxGroupInput("NF", "Variables to show:",c("비농업고용지수"="NFPTCH","ADP고용"="ADPNFP"),selected="NFTCH")),
                                         dashboardBody(fluidRow(box(plotOutput("BM"), width =12, solidHeader = TRUE)),
                                                       fluidRow(box(plotOutput("ASSETEOY"), width =12, solidHeader = TRUE)),
                                                       fluidRow(box(DTOutput("PA"), width =6, solidHeader = TRUE))
                                         ))))
server <- function(input, output){
  output$ASSET   <- renderPlot({
    RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT"|variable=="GOLD")%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%trans_rt("week")%>%dt_trans%>%
      mutate(USGOVT=3*USGOVT)%>%mutate(SP500=3*SP500)%>%
      mutate(BM=0.6*SP500+0.4*USGOVT)%>%exname%>%cuml%>%cplot+
      theme(legend.text = element_text(size=15))
  })
  output$BM   <- renderPlot({
    RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT"|variable=="GOLD")%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%trans_rt("week")%>%dt_trans%>%
      mutate(BM =0.5*SP500+0.5*USGOVT)%>%
      mutate(BM2=0.6*SP500+0.4*USGOVT)%>%
      mutate(BM3=0.4*SP500+0.6*USGOVT)%>%
      cuml%>%cplot+theme(legend.text = element_text(size=15))
  })
  output$ASSETEOY   <- renderPlot({
    RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%trans_rt("day")%>%dt_trans%>%
      mutate(BM =0.6*SP500+0.4*USGOVT)%>%
      mutate(BM2=0.5*SP500+0.5*USGOVT)%>%
      mutate(BM3=0.4*SP500+0.6*USGOVT)%>%
      apply.yearly(., Return.cumulative)%>%na.omit%>%dt_trans%>%graphbar2("WEEK")+
      theme(legend.text = element_text(size=15))
  })
  output$PA   <- renderDT({
    RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%trans_rt("month")%>%dt_trans%>%
      mutate(BM =0.6*SP500+0.4*USGOVT)%>%
      mutate(BM2=0.5*SP500+0.5*USGOVT)%>%
      mutate(BM3=0.4*SP500+0.6*USGOVT)%>%PA
  })
}
shinyApp(ui, server)

#ROLL_COR
tmp   <- RAWDATA%>%filter(variable=="SP500"|variable=="KOSPI"|variable=="USGOVT"|variable=="KRBOND")%>%dcast(STD_DT~variable)%>% trans_rt("month")%>%dt_trans
ttmp  <- RAWDATA%>%filter(variable=="SP500"|variable=="KOSPI"|variable=="USGOVT"|variable=="KRBOND")%>%dcast(STD_DT~variable)%>% trans_rt("week")%>%dt_trans
#ROLLING CORRELATION 5YEAR
data.table(STD_DT=tmp$STD_DT,ROLLIMG5YEARWR =roll_cor(tmp$SP500, tmp$USGOVT, width = 36))
data.table(STD_DT=tmp$STD_DT,ROLLIMG5YEARWR =roll_cor(tmp$KOSPI, tmp$KRBOND, width = 36))
RES4 <- data.table(STD_DT=tmp$STD_DT,ROLLIMG5YEARWR =roll_cor(tmp$SP500, tmp$USGOVT, width = 60),ROLLIMG5YEARUS =roll_cor(tmp$KOSPI, tmp$KRBOND, width = 60))
RES4%>%cplot

RAWDATA%>%filter(variable=="USLABOR")%>%arrange(STD_DT)
RAWDATA%>%filter(variable=="AHEYOY")%>%arrange(STD_DT)%>%dcast(STD_DT~variable)%>% cplot
RAWDATA%>%filter(variable=="NFPTCH")%>%arrange(STD_DT)%>%dcast(STD_DT~variable)%>% cplot
RAWDATA%>%filter(variable=="ADPNFP")%>%arrange(STD_DT)%>%dcast(STD_DT~variable)%>% cplot
#노동, 고용지표
ui <-  navbarPage("Dashboard", theme = shinytheme("flatly"),
       tabPanel("고용지표",
        dashboardPage(dashboardHeader(),
        dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('1970-01-01') , end = as.Date('2023-12-31')),
                         checkboxGroupInput("VAR1", "Variables to show:",c("미국실업률"="USLABOR","임금인상(YoY)"="AHEYOY","임금인상(MoM)"="AHEMOM",selected="USLABOR"),
                         checkboxGroupInput("VAR2", "Variables to show:",c("비농업고용지수"="NFPTCH","ADP고용"="ADPNFP"),selected="NFTCH")),
        dashboardBody(fluidRow(box(plotOutput("TMP1"), width =12, solidHeader = TRUE)),
                      fluidRow(box(plotOutput("TMP2"), width =12, solidHeader = TRUE))
                      ))))

server <- function(input, output){
   output$TMP1   <- renderPlot({
   RAWDATA%>%filter(variable==input$VAR1)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%
          cplot()
   })
   output$TMP2   <- renderPlot({
   RAWDATA%>%filter(variable==input$VAR2)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%
        mutate(rolling_avg60 = rollmean(NFPTCH, k=12, fill=NA, align='right'))%>%
        #mutate(rolling_avg36 = rollmean(ADPNFP, k=12, fill=NA, align='right'))%>%
        #mutate(rolling_avg24 = rollmean(NFPTCH, k=24, fill=NA, align='right'))%>%
        #mutate(rolling_avg12 = rollmean(NFPTCH, k=12, fill=NA, align='right'))%>%
        exname%>%cplot + coord_cartesian( ylim = c(-1100, 1100))
   })
}

shinyApp(ui, server) 

RAWDATA%>%filter(variable=="USLABOR")

RAWDATA%>%filter(variable=="USLABOR")%>%dcast(STD_DT~variable)%>%mutate(Base=min(USLABOR))%>%cplot

RAWDATA%>%filter(variable==input$NF)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%
  mutate(rolling_avg60 = rollmean(NFPTCH, k=60, fill=NA, align='right'))%>%
  mutate(rolling_avg36 = rollmean(NFPTCH, k=36, fill=NA, align='right'))%>%
  mutate(rolling_avg24 = rollmean(NFPTCH, k=24, fill=NA, align='right'))%>%
  mutate(rolling_avg12 = rollmean(NFPTCH, k=12, fill=NA, align='right'))%>%
  exname

#통화정책효과분석
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

#장기수익률률 벡테스팅
ui <-     navbarPage("Dashboard", theme = shinytheme("flatly"),
                     tabPanel("실질질금리",dashboardPage(dashboardHeader(),
                                                    dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2018-01-01') , end = as.Date('2022-12-31')),
                                                                     checkboxGroupInput("rr", "Variables to show:",c("금"="GOLD","실질금리"="RINTEREST","TIP10Y"="TIP10Y","기준금리"="USBR"
                                                                     ),selected="INF")),
                                                    dashboardBody(fluidRow(box(plotOutput("INTEREST"), width =12, solidHeader = TRUE)),
                                                                  fluidRow(box(plotOutput("BACKTEST"), width =12, solidHeader = TRUE)),
                                                                  fluidRow(box(plotOutput("GDP"), width =12, solidHeader = TRUE)),
                                                                  fluidRow(box(DTOutput("rty"), width =6, solidHeader = TRUE),box(DTOutput("PA"), width =6, solidHeader = TRUE))
                                                    ))))
server <- function(input, output){
  output$BACKTEST      <- renderPlot({
    RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%mutate(BM=0.6*SP500+0.4*USGOVT)%>%
      mutate(USGOVT=USGOVT)%>%mutate(SP500=SP500)%>%trans_rt("week")%>%dt_trans%>%exname%>%cuml%>%cplot
  })
  output$GDP       <- renderPlot({
    RAWDATA%>%filter(variable=="USGDPY")%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%exname%>%cplot
  })
  output$INTEREST     <- renderPlot({
    RAWDATA%>%filter(variable=="USBR"|variable=="US10Y")%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%exname%>%cplot
  })
  output$rty      <- renderPlot({
                     RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%mutate(BM=0.6*SP500+0.4*USGOVT)%>%
                     mutate(USGOVT=USGOVT)%>%mutate(SP500=SP500)%>%trans_rt("year")%>%dt_trans%>%graphbar2("EOY")
  })
  output$PA       <- renderDT({
                     RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans%>%
                     mutate(USGOVT=USGOVT)%>%mutate(SP500=SP500)%>%mutate(BM=0.6*SP500+0.4*USGOVT)%>%PA
  })
}
shinyApp(ui, server)   

tmp <- RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%mutate(BM=0.6*SP500+0.4*USGOVT)%>%
  mutate(USGOVT=USGOVT)%>%mutate(SP500=SP500)%>%trans_rt("year")%>%dt_trans
write.xlsx(tmp ,"c:/work/monthly.xlsx", sheetName="PMI",append=F)

#금리
 ui <-       navbarPage("Dashboard", theme = shinytheme("flatly"),
             tabPanel("기준금리",dashboardPage(dashboardHeader(),
                                               dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2018-01-01') , end = as.Date('2022-12-31')),
                                                                checkboxGroupInput("country", "Variables to show:",c("미국"="USBR","한국"="KRBR","중국"="CNBR","유로"="EUBR","일본"="JPBR","영국"="UKBR","금"="GOLD",
                                                                                                                     "호주"="AUBR","브라질"="BRBR","인도"="INBR","미국10년물"="US10Y","미국2년물"="US2Y"),selected="USBR"),
                                                                checkboxGroupInput("country2", "Variables to show:",c("미국10년물"="US10Y","한국10년물"="KR10Y","중국10년물"="CN10Y","독일10년물"="DEM10Y",
                                                                                                                      "일본10년물"="JP10Y","영국10년물"="UK10Y","프랑스10년"="FR10Y","이탈리아10년물"="IT10Y")
                                                                                   ,selected="US10Y")),
                                               dashboardBody(fluidRow(box(plotOutput("baser"), width =12, solidHeader = TRUE)),
                                                             fluidRow(box(plotOutput("country"), width =12, solidHeader = TRUE))
                                               ))))
  server <- function(input, output){
  output$baser   <- renderPlot({RAWDATA%>%filter(variable==input$country)%>%filter(STD_DT>input$range)%>%dcast(STD_DT~variable)%>%exname%>%cplot+
                    theme(legend.text = element_text(size=15))
  })
  output$country <- renderPlot({RAWDATA%>%filter(variable==input$country2)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%exname%>%cplot+
                    theme(legend.text = element_text(size=15))
  })
  }
  shinyApp(ui, server)
  
  
#실질금리
  
  ui <-     navbarPage("Dashboard", theme = shinytheme("flatly"),
                       tabPanel("실질질금리",dashboardPage(dashboardHeader(),
                                                     dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2018-01-01') , end = as.Date('2022-12-31')),
                                                                      checkboxGroupInput("rr", "Variables to show:",c("금"="GOLD","미국채10년"="US10Y","TIP10Y"="TIP10Y","기준금리"="USBR","BEI"="BEI10Y","미국CPI"="USCPIYOY"
                                                                      ),selected="INF")),
                                                     dashboardBody(fluidRow(box(plotOutput("RR"), width =12, solidHeader = TRUE))
                                                     ))))
  server <- function(input, output){
    output$RR   <- renderPlot({
        RAWDATA%>%filter(variable==input$rr)%>%filter(STD_DT>input$range)%>%dcast(STD_DT~variable)%>%exname%>%cplot+
        theme(legend.text = element_text(size=15))
    })
   }
  
  shinyApp(ui, server)
  
  
#스프레드
 ui <-       navbarPage("Dashboard", theme = shinytheme("flatly"),
             tabPanel("기준금리",dashboardPage(dashboardHeader(),
               dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2018-01-01') , end = as.Date('2022-12-31')),
                checkboxGroupInput("sp", "Variables to show:",c("미국HY"="HYSP","신흥국"="EMSP","미국IG"="IGSPAA"),selected="HYSP")),
               dashboardBody(fluidRow(box(plotOutput("SP"), width =12, solidHeader = TRUE))
                             ))))
  server <- function(input, output){
    output$SP   <- renderPlot({RAWDATA%>%filter(variable==input$sp)%>%filter(STD_DT>input$range)%>%dcast(STD_DT~variable)%>%exname%>%cplot+
        theme(legend.text = element_text(size=15))
    })
    output$country <- renderPlot({RAWDATA%>%filter(variable==input$country2)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%exname%>%cplot+
        theme(legend.text = element_text(size=15))
    })
  }
  
  shinyApp(ui, server)

  
# pmi
ui <-     navbarPage("Dashboard", theme = shinytheme("flatly"),
            tabPanel("PMI",dashboardPage(dashboardHeader(),
            dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2020-01-01') , end = as.Date('2023-12-31')),
            checkboxGroupInput("pmi", "Variables to show:",c("ISM제조업"="ISMPMIM","ISM서비스"="ISMPMIS","미국제조업"="USPMI","유럽"="EUPMI","한국PMI"="KRPMI","중국PMI"="CNPMI","미국서비스"="USPMIS"),selected="KRPMI")),
            dashboardBody(fluidRow(box(plotOutput("pmi")  , width =12, solidHeader = TRUE))
                          ))),
            tabPanel("CLI",dashboardPage(dashboardHeader(),
            dashboardSidebar(dateRangeInput('range2',label = '',start = as.Date('2020-01-01') , end = as.Date('2023-12-31')),
            checkboxGroupInput("cli", "Variables to show:",c("미국"="USA","중국"="CHN","한국"="KOR","일본"="JPN","영국"="GBK","세계"="OECD"),selected="OECD")),
            dashboardBody(fluidRow(box(plotOutput("cli"), width =12, solidHeader = TRUE)))
            ))
            )
server <- function(input, output){   

  output$pmi      <- renderPlot({
                     RAWDATA%>%filter(variable==input$pmi)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%mutate(기준선=50)%>%exname%>%cplot+theme(legend.text = element_text(size=15))
  })
  output$cli      <- renderPlot({
                     RAWDATA%>%filter(variable==input$cli)%>%filter(STD_DT>input$range2[1]&STD_DT<input$range2[2])%>%dcast(STD_DT~variable)%>%mutate(기준선=100)%>%exname%>%cplot+
                     theme(legend.text = element_text(size=15))
  })
  output$stock    <- renderPlot({
                     RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%trans_rt("week")%>%dt_trans%>%
                     mutate(USGOVT=3*USGOVT)%>%mutate(SP500=3*SP500)%>%
                     mutate(BM=0.6*SP500+0.4*USGOVT)%>%exname%>%cuml%>%cplot+
                     theme(legend.text = element_text(size=15))
  })
  # output$rty      <- renderPlot({
  #                    RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%dcast(STD_DT~variable)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%
  #                    mutate(USGOVT=USGOVT)%>%mutate(SP500=SP500)%>%trans_rt("year")%>%dt_trans%>%graphbar2("EOY")
  # })
  # output$PA       <- renderDT({
  #                    RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%trans_rt("week")%>%dt_trans%>%
  #                    mutate(USGOVT=USGOVT)%>%mutate(SP500=SP500)%>%mutate(BM=0.6*SP500+0.4*USGOVT)%>%exname%>%PA
  # })
  
}
shinyApp(ui, server)
RAWDATA%>%filter(variable=="ISMPMIS")
ISMPMIS
#cli
ui <-     navbarPage("Dashboard", theme = shinytheme("flatly"),
                     tabPanel("CLI",dashboardPage(dashboardHeader(),
                                                  dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2020-01-01') , end = as.Date('2023-12-31')),
                                                                   checkboxGroupInput("cli", "Variables to show:",c("미국"="USA","중국"="CHN","한국"="KOR","일본"="JPN","영국"="GBK","세계"="OECD"),selected="OECD")),
                                                  dashboardBody(fluidRow(box(plotOutput("cli"), width =12, solidHeader = TRUE)),
                                                                fluidRow(box(plotOutput("stock"), width =12, solidHeader = TRUE)))
                     ))
)
server <- function(input, output){   
         output$cli      <- renderPlot({
                              RAWDATA%>%filter(variable==input$cli)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%mutate(기준선=100)%>%exname%>%cplot+
                              theme(legend.text = element_text(size=15))
          })
          output$stock    <- renderPlot({
            
                             RAWDATA%>%filter(variable=="SP500"|variable=="USGOVT")%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%dcast(STD_DT~variable)%>%trans_rt("week")%>%dt_trans%>%
                             mutate(USGOVT=3*USGOVT)%>%mutate(SP500=3*SP500)%>%
                             mutate(BM=0.6*SP500+0.4*USGOVT)%>%exname%>%cuml%>%cplot+
                             theme(legend.text = element_text(size=15))
                             })
}
shinyApp(ui, server)


#매크로팩터

ui <-     navbarPage("Dashboard", theme = shinytheme("flatly"),
                     tabPanel("기준금리",dashboardPage(dashboardHeader(),
                                                   dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2018-01-01') , end = as.Date('2022-12-31')),
                                                                    checkboxGroupInput("mf", "Variables to show:",c("인플레이션"="INF","실질금리"="RINTEREST","신용"="CREDIT","환율"="USDKRW","성장"="GROWTH"
                                                                                                                         ),selected="INF")),
                                                   dashboardBody(fluidRow(box(plotOutput("CUMPLOT_MF"), width =12, solidHeader = TRUE)),
                                                                 fluidRow(box(plotOutput("CPI"), width =12, solidHeader = TRUE))
                                                   ))))
{    output$CUMPLOT_MF <- renderPlot({
    retm%>%dplyr::select(STD_DT,input$mf)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%cuml%>%exname%>%cplot+
    theme(legend.text = element_text(size=15))
})

output$CPI <- renderPlot({

  RAWDATA%>%filter(variable=="USCPIYOY"|variable=="USBR")%>%dcast(STD_DT~variable)%>%filter(STD_DT>input$range[1]&STD_DT<input$range[2])%>%exname%>%cplot  +
    theme(legend.text = element_text(size=15))
})

}

shinyApp(ui, server)

#물가

ui <-       navbarPage("Dashboard", theme = shinytheme("flatly"),
                       tabPanel("기준금리",dashboardPage(dashboardHeader(),
                                                     dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2018-01-01') , end = as.Date('2022-12-31')),
                                                                      checkboxGroupInput("country", "Variables to show:",c("미국"="USCPIYOY","한국"="KRCPIYOY","중국"="CNCPIYOY","유로"="EUCPI","일본"="JPCPIYOY",
                                                                                                                           "영국"="UKCP"),selected="USCPIYOY")),
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
readxl::read_excel("c:/work/MACRO.xlsx",sheet="monthly")%>%handling%>%as.data.table%>%melt(id.vars="STD_DT")%>%na.omit%>%filter(variable=="UKCPIYOY")
RAWDATA%>%filter(variable=="KRCPIYOY")
#미국 서비스업 PMI  
RAWDATA%>%filter(variable=="ISMPMIM"|variable=="ISMPMIS")%>%dcast(STD_DT~variable)%>%filter(STD_DT>"2018-01-01"&STD_DT<"2023-08-31")%>%mutate(기준선=50)%>%exname%>%cplot
RAWDATA%>%filter(variable=="USCPIYOY"|variable=="USCORECPIYOY"|variable=="USPCEYOY"|variable=="bei10y")

#미국 소비자 물가
RAWDATA%>%filter(variable=="USCPIYOY"|variable=="USCORECPIYOY"|variable=="USPCEYOY"|variable=="USBR")%>%dcast(STD_DT~variable)%>%filter(STD_DT>"1990-01-01"&STD_DT<"2023-07-30")%>%exname%>%cplot  

#금리
RAWDATA%>%filter(variable=="US10Y"|variable=="CN10Y"|variable=="KR10Y"|variable=="UK10Y")%>%dcast(STD_DT~variable)%>%exname%>%cplot  

#VIX, MOVE
RAWDATA%>%filter(variable=="EMSP")%>%filter(STD_DT>"2020-01-01")%>%dcast(STD_DT~variable)%>%cplot
VIXMOVE <- RAWDATA%>%filter(variable=="VIX"|variable=="MOVE")%>%filter(STD_DT>"2020-01-01")%>%dcast(STD_DT~variable)
VIXMOVE %>% cplot
write.xlsx(ret ,"c:/work/monthly.xlsx", sheetName="VIXMOVE",append=F)
write.xlsx(BASER ,"c:/work/monthly.xlsx", sheetName="BASERATE",append=T)
#OECD
OECD <- RAWDATA%>%filter(variable=="USA"|variable=="DEU"|variable=="GBR"|variable=="KOR"|variable=="CHN")%>%filter(STD_DT>"2010-01-01")%>%dcast(STD_DT~variable)

#CITI SURPRIRES
CITI <- RAWDATA%>%filter(variable=="USA"|variable=="DEU"|variable=="GBR"|variable=="KOR"|variable=="CHN")%>%filter(STD_DT>"2010-01-01")%>%dcast(STD_DT~variable)

#제조업,서비스업 PMI

RAWDATA%>%filter(variable=="ISMPMIM"|variable=="ISMPMIS"|variable=="ISMUSPMIT")%>%dcast(STD_DT~variable)%>%na.omit%>%cplot
RAWDATA%>%filter(variable=="EUPMI"|variable=="EUPMIS"|variable=="EUPMIT")%>%dcast(STD_DT~variable)%>%na.omit%>%cplot
RAWDATA%>%filter(variable=="DEPMI"|variable=="DEPMIS"|variable=="DEPMIT")%>%dcast(STD_DT~variable)%>%na.omit%>%cplot
RAWDATA%>%filter(variable=="MSEU"|variable=="DAX"|variable=="EURO50")%>%filter(STD_DT>"2021-01-01")%>%dcast(STD_DT~variable)%>%trans_rt("week")%>%dt_trans%>%cuml%>%cplot()
RAWDATA%>%filter(variable=="MSEU"|variable=="DAX"|variable=="EURO50")%>%filter(STD_DT>"2021-01-01")%>%dcast(STD_DT~variable)%>%trans_rt("year")
PMI <- RAWDATA%>%filter(variable=="MSEU"|variable=="DAX"|variable=="EURO50")%>%filter(STD_DT>"2021-01-01")%>%dcast(STD_DT~variable)%>%trans_rt("week")%>%dt_trans%>%cuml%>%
full_join(RAWDATA%>%filter(variable=="DEPMI"|variable=="DEPMIS"|variable=="DEPMIT"|variable=="EUPMI"|variable=="EUPMIS"|variable=="EUPMIT")%>%dcast(STD_DT~variable),by="STD_DT")%>%arrange(STD_DT)

write.xlsx(PMI ,"c:/work/monthly.xlsx", sheetName="PMI",append=T)


  
#CREDIT 스프데르  

RAWDATA%>%filter(variable=="HYSP")%>%dcast(STD_DT~variable)
TMP%>%filter(variable=="HYSP")%>%na.omit%>%arrange(STD_DT)%>%dcast(STD_DT~variable)
TMP%>%filter(variable=="HYSP")




#무역수지, 수출, 코스피

RAWDATA%>%filter(variable=="KORSTY"|variable=="KOTRBAL")%>%dcast(STD_DT~variable) %>%left_join(
RAWDATA%>%filter(variable=="KOSPI")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans,by="STD_DT" )%>%na.omit %>%dplyr::select(-STD_DT)%>%cor

RAWDATA%>%filter(variable=="KOTRBAL")%>%dcast(STD_DT~variable) %>%left_join(
RAWDATA%>%filter(variable=="KOSPI")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans,by="STD_DT" )%>%na.omit %>% filter(KOTRBAL<0) %>%dplyr::select(-STD_DT)%>%apply(2,mean)%>%round(3)*12

#주식시장 정리
ui <-   navbarPage("Dashboard", theme = shinytheme("flatly"),
        tabPanel("주요주가지수",dashboardPage(dashboardHeader(title = "View"),dashboardSidebar(dateInput("date",h3("Date input"),value = "2023-09-01")),
                                dashboardBody(fluidRow(shinydashboard::valueBox("WEEK","주가지수,EPS",color="red", width = 4),
                                                       shinydashboard::valueBox("MONTH","주가지수,EPS",color="blue", width = 4),
                                                       shinydashboard::valueBox("YTD","주가지수,EPS",color="orange", width = 4)),
                                              fluidRow(box(plotOutput("stock_wek"), width = 4, solidHeader = TRUE,background = "red"),
                                                       box(plotOutput("stock_mon"), width = 4, solidHeader = TRUE,background = "blue"),
                                                       box(plotOutput("stock_ytd"), width = 4, solidHeader = TRUE,background = "orange")),
                                              fluidRow(box(plotOutput("bond_wek"), width = 4, solidHeader = TRUE,background = "red"),
                                                       box(plotOutput("bond_mon"), width = 4, solidHeader = TRUE,background = "blue"),
                                                       box(plotOutput("bond_ytd"), width = 4, solidHeader = TRUE,background = "orange")),
                                              fluidRow(box(plotOutput("al_wek"), width = 4, solidHeader = TRUE,background = "red"),
                                                       box(plotOutput("al_mon"), width = 4, solidHeader = TRUE,background = "blue"),
                                                       box(plotOutput("al_ytd"), width = 4, solidHeader = TRUE,background = "orange"))
                        ))))


server <- function(input, output)
{   
  output$stock_wek <- renderPlot({graphbar(stock_wek%>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("week") %>%tail(n=1)%>%exname%>%dt_trans,"red"   ,"WEEK")})
  output$stock_mon <- renderPlot({graphbar(stock_mon%>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%tail(n=1)%>%exname%>%dt_trans,"blue"  ,"MONTH")})
  output$stock_ytd <- renderPlot({graphbar(stock_ytd%>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("year") %>%tail(n=1)%>%exname%>%dt_trans,"orange","YTD")})
  output$bond_wek  <- renderPlot({graphbar(bond_wek %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("week") %>%tail(n=1)%>%exname%>%dt_trans,"red"   ,"WEEK")})
  output$bond_mon  <- renderPlot({graphbar(bond_mon %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%tail(n=1)%>%exname%>%dt_trans,"blue"  ,"MONTH")})
  output$bond_ytd  <- renderPlot({graphbar(bond_ytd %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("year") %>%tail(n=1)%>%exname%>%dt_trans,"orange","YTD")})
  output$al_wek    <- renderPlot({graphbar(al_wek   %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("week") %>%tail(n=1)%>%exname%>%dt_trans,"red"   ,"WEEK")})
  output$al_mon    <- renderPlot({graphbar(al_mon   %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%tail(n=1)%>%exname%>%dt_trans,"blue"  ,"MONTH")})
  output$al_ytd    <- renderPlot({graphbar(al_ytd   %>%filter(STD_DT<input$date)%>%dcast(STD_DT~variable)%>%trans_rt("year") %>%tail(n=1)%>%exname%>%dt_trans,"orange","YTD")})
  
  
}
shinyApp(ui, server)
bond_wek %>%dcast(STD_DT~variable)%>%trans_rt("week")

stock_wek <- RAWDATA%>%filter(variable=="WORLD"|variable=="DOW"|variable=="NASDAQ"|variable=="MSEU"|variable=="MSUK"|variable=="MEDE"|variable=="MEFR"|variable=="MSIN"|variable=="MSBR"|
                                variable=="MSAU"|variable=="MSCA"|variable=="KOSPI"|variable=="KOSDAQ"|variable=="MSJP"|variable=="SHANGHAI"|variable=="CSI300"|variable=="HANGS"|variable=="JEPI.Adjusted")

stock_mon <- RAWDATA%>%filter(variable=="WORLD"|variable=="DOW"|variable=="NASDAQ"|variable=="MSEU"|variable=="MSUK"|variable=="MEDE"|variable=="MEFR"|variable=="MSIN"|variable=="MSBR"|
                              variable=="MSAU"|variable=="MSCA"|variable=="KOSPI"|variable=="KOSDAQ"|variable=="MSJP"|variable=="SHANGHAI"|variable=="CSI300"|variable=="HANGS"|variable=="JEPI.Adjusted")

stock_ytd <- RAWDATA%>%filter(variable=="WORLD"|variable=="DOW"|variable=="NASDAQ"|variable=="MSEU"|variable=="MSUK"|variable=="MEDE"|variable=="MEFR"|variable=="MSIN"|variable=="MSBR"|
                              variable=="MSAU"|variable=="MSCA"|variable=="KOSPI"|variable=="KOSDAQ"|variable=="MSJP"|variable=="SHANGHAI"|variable=="CSI300"|variable=="HANGS"|variable=="JEPI.Adjusted")

             grid.arrange(graphbar(stock_mon,"steelblue","주식MON"),graphbar(stock_ytd,"steelblue","주식YTD"),ncol=2)
             write.xlsx(stock_mon ,"c:/work/monthly.xlsx", sheetName="stock_mon",append=T)
             write.xlsx(stock_ytd ,"c:/work/monthly.xlsx", sheetName="stock_ytd",append=T)
#주식시장 정리
            
FEPS_mon <- RAWDATA%>%filter(variable=="FEPS_WORLD"|variable=="FEPS_DOW"|variable=="FEPS_NASDAQ"|variable=="FEPS_MSEU"|variable=="FEPS_MSUK"|variable=="FEPS_MEDE"|variable=="FEPS_MEFR"|variable=="FEPS_MSIN"|variable=="FEPS_MSBR"|
                             variable=="FEPS_MSAU"|variable=="FEPS_MSCA"|variable=="FEPS_KOSPI"|variable=="FEPS_KOSDAQ"|variable=="FEPS_MSJP"|variable=="FEPS_SHANGHAI"|variable=="FEPS_CSI300"|variable=="FEPS_HANGS")%>%
            filter(STD_DT<input$date)%>%
            dcast(STD_DT~variable)%>%trans_rt("month")%>%tail(n=1)%>%exname%>%dt_trans

FEPS_ytd <- RAWDATA%>%filter(variable=="FEPS_WORLD"|variable=="FEPS_DOW"|variable=="FEPS_NASDAQ"|variable=="FEPS_MSEU"|variable=="FEPS_MSUK"|variable=="FEPS_MEDE"|variable=="FEPS_MEFR"|variable=="FEPS_MSIN"|variable=="FEPS_MSBR"|
                             variable=="FEPS_MSAU"|variable=="FEPS_MSCA"|variable=="FEPS_KOSPI"|variable=="FEPS_KOSDAQ"|variable=="FEPS_MSJP"|variable=="FEPS_SHANGHAI"|variable=="FEPS_CSI300"|variable=="FEPS_HANGS")%>%
            filter(STD_DT<input$date)%>%
            dcast(STD_DT~variable)%>%trans_rt("year")%>%tail(n=1)%>%exname%>%dt_trans
   
            grid.arrange(graphbar(FEPS_mon,"steelblue","주식"),graphbar(FEPS_ytd,"steelblue","FEPS"),ncol=2)
            
            write.xlsx(FEPS_mon ,"c:/work/monthly.xlsx", sheetName="FEPS_mon",append=T)
            write.xlsx(FEPS_ytd ,"c:/work/monthly.xlsx", sheetName="FEPS_ytd",append=T)

#채권시장 정리
bond_wek <- RAWDATA%>%filter(variable=="WRBOND"|variable=="USBOND"|variable=="USGOVT"|variable=="USIG"|variable=="USHY"|variable=="WRIG"|variable=="WRHY"|variable=="CNBOND"|variable=="KRBOND"|variable=="EUHY"|
                                           variable=="EUIG"|variable=="USSHORT"|variable=="USLONG"|variable=="WRGOVT"|variable=="EUBOND"|variable=="EMGOVT"|variable=="EMGOVTL"|variable=="EMBOND")

bond_mon <- RAWDATA%>%filter(variable=="WRBOND"|variable=="USBOND"|variable=="USGOVT"|variable=="USIG"|variable=="USHY"|variable=="WRIG"|variable=="WRHY"|variable=="CNBOND"|variable=="KRBOND"|variable=="EUHY"|
                                            variable=="EUIG"|variable=="USSHORT"|variable=="USLONG"|variable=="WRGOVT"|variable=="EUBOND"|variable=="EMGOVT"|variable=="EMGOVTL"|variable=="EMBOND")
bond_mon%>%graphbar("orange","Montly return")           
bond_ytd <- RAWDATA%>%filter(variable=="WRBOND"|variable=="USBOND"|variable=="USGOVT"|variable=="USIG"|variable=="USHY"|variable=="WRIG"|variable=="WRHY"|variable=="CNBOND"|variable=="KRBOND"|variable=="EUHY"|
                               variable=="EUIG"|variable=="USHORT"|variable=="USLONG"|variable=="WRGOVT"|variable=="EUBOND"|variable=="EMGOVT"|variable=="EMGOVTL"|variable=="EMBOND")
            
            grid.arrange(graphbar(bond_mon,"steelblue","채권MON"),graphbar(bond_ytd,"steelblue","채권YTD"),ncol=2)
            
            write.xlsx(bond_mon ,"c:/work/monthly.xlsx", sheetName="bond_mon",append=T)
            write.xlsx(bond_ytd ,"c:/work/monthly.xlsx", sheetName="bond_ytd",append=T)
            
            
#대체투자자

            
al_wek   <- RAWDATA%>%filter(variable=="PEF"|variable=="USREIT"|variable=="WRREIT"|variable=="CRB"|variable=="WRINFRA"|variable=="WREPRA"|variable=="GOLD"|variable=="WTI"|variable=="USGAS"|variable=="EUGAS"|
                                           variable=="BITC"|variable=="COP"|variable=="SIL"|variable=="SPAR"|variable=="HFRU"|variable=="USINFRA"|variable=="EUINFRA"|variable=="UKGAS"|variable=="SPAR")
                        
al_mon   <- RAWDATA%>%filter(variable=="PEF"|variable=="USREIT"|variable=="WRREIT"|variable=="CRB"|variable=="WRINFRA"|variable=="WREPRA"|variable=="GOLD"|variable=="WTI"|variable=="USGAS"|variable=="EUGAS"|
                             variable=="BITC"|variable=="COP"|variable=="SIL"|variable=="SPAR"|variable=="HFRU"|variable=="USINFRA"|variable=="EUINFRA"|variable=="UKGAS"|variable=="SPAR")



al_ytd   <- RAWDATA%>%filter(variable=="PEF"|variable=="USREIT"|variable=="WRREIT"|variable=="CRB"|variable=="WRINFRA"|variable=="WREPRA"|variable=="GOLD"|variable=="WTI"|variable=="USGAS"|variable=="EUGAS"|
                               variable=="BITC"|variable=="COP"|variable=="SIL"|variable=="SPAR"|variable=="HFRU"|variable=="USINFRA"|variable=="EUINFRA"|variable=="UKGAS"|variable=="SPAR")

            grid.arrange(graphbar(al_mon,"steelblue","채권MON"),graphbar(al_ytd,"steelblue","채권YTD"),ncol=2)
            
            write.xlsx(al_mon ,"c:/work/monthly.xlsx", sheetName="al_mon",append=T)
            write.xlsx(al_ytd ,"c:/work/monthly.xlsx", sheetName="al_ytd",append=T)
                        
#외환
            RAWDATA%>%filter(variable=="DXY"|variable=="USDKRW"|variable=="EURO"|variable=="CNH"|variable=="EMX"|variable=="GBP"|variable=="YEN")%>%
            dcast(STD_DT~variable)%>%filter(STD_DT=="2023-08-01")
            