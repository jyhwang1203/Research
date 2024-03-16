
ui <- dashboardPage(
  dashboardHeader(title = "MACRO FACTOR MODEL"),
  dashboardSidebar(dateRangeInput('date',label = '',start = as.Date('2010-01-01') , end = as.Date('2023-12-31'))
                    ),
  dashboardBody(fluidRow(box(plotOutput("MACRO"), width = 8, solidHeader = TRUE,background = "blue"),
                box(DTOutput("MACRO2"), width = 4, solidHeader = TRUE)
    ),
    fluidRow(box(plotOutput("MACRO3"), width = 3, solidHeader = TRUE,background = "blue"),
             box(plotOutput("MACRO4"), width = 3, solidHeader = TRUE,background = "blue"),
             box(plotOutput("MACRO5"), width = 3, solidHeader = TRUE,background = "blue"),
             box(plotOutput("MACRO6"), width = 3, solidHeader = TRUE,background = "blue")
    )
    ,
    fluidRow(box(plotOutput("MACRO7"), width = 3, solidHeader = TRUE,background = "blue"),
             box(plotOutput("MACRO8"), width = 3, solidHeader = TRUE,background = "blue"),
             box(plotOutput("MACRO9"), width = 3, solidHeader = TRUE,background = "blue"),
             box(plotOutput("MACRO10"), width = 3, solidHeader = TRUE,background = "blue")
    ),
    fluidRow(box(plotOutput("MACRO11"), width = 3, solidHeader = TRUE,background = "blue"),
             box(plotOutput("MACRO12"), width = 3, solidHeader = TRUE,background = "blue"),
                      box(plotOutput("MACRO13"), width = 3, solidHeader = TRUE,background = "blue"),
                               box(plotOutput("MACRO14"), width = 3, solidHeader = TRUE,background = "blue")
    ),
    fluidRow(box(plotOutput("MACRO15"), width = 3, solidHeader = TRUE,background = "blue"),
             box(plotOutput("MACRO16"), width = 3, solidHeader = TRUE,background = "blue"),
             box(plotOutput("MACRO17"), width = 3, solidHeader = TRUE,background = "blue")
    )
    )
  )



server <- function(input, output) {
  
  output$MACRO <- renderPlot({
    retm %>% dplyr::select(STD_DT,GROWTH,RINTEREST,INF,CREDIT,FX) %>%filter(STD_DT>input$date[1]&STD_DT<input$date[2]) %>%cuml %>%cplot
  })
  output$MACRO2 <- renderDT({
    retm %>% dplyr::select(STD_DT,GROWTH,RINTEREST,INF,CREDIT,FX) %>%filter(STD_DT>input$date[1]&STD_DT<input$date[2]) %>%PA
  })
  output$MACRO3 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
      EXPOSURE %>% melt %>% filter(substr(Var1,1,5)=="WORLD")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))  + ggtitle("글로벌주식") 
  })
  output$MACRO4 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,4)=="MSKR")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("한국주식") 
  })
  output$MACRO5 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,6)=="WRBOND")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("글로벌채권") 
  })
  output$MACRO6 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,6)=="KRBOND")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("한국채권") 
  })
  output$MACRO7 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,6)=="WREPRA")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("글로벌부동산") 
  })
  output$MACRO8 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,7)=="WRINFRA")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("글로벌인프라") 
  })
  output$MACRO9 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,4)=="GSCI")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("원자재") 
  })
  output$MACRO10 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,3)=="PEF")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("사모펀드") 
  })
  output$MACRO11 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,2)=="EM")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("신흥국주식") 

  })
  output$MACRO12 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,6)=="USGOVT")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("미국국채") 
  })
  output$MACRO13 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,4)=="USIG")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("미국투자적격") 
  })
  output$MACRO14 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,4)=="USHY")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))   + ggtitle("미국하이일드") 
  })

  output$MACRO15 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,3)=="WTI")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))  + ggtitle("WTI원유") 
  })
  output$MACRO16 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,4)=="WRREIT")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))  + ggtitle("금") 
  })
  output$MACRO17 <- renderPlot({
    UNIV_MACRO(input$date[1],input$date[2])
    EXPOSURE %>%melt %>% filter(substr(Var1,1,4)=="USREIT")%>%
      ggplot(aes(x=Var2, y=value, col=Var1,fill=Var1)) +  
      geom_bar(position="dodge", stat="identity") +theme(axis.text.x = element_text(size = 20,angle=90))  + ggtitle("헤지펀드") 
  })
}

shinyApp(ui = ui, server = server)
TMP <- retm %>% dplyr::select(STD_DT,GROWTH,RINTEREST,INF,CREDIT,FX) %>%filter(STD_DT>input$date[1]&STD_DT<input$date[2])
UNIV_MACRO("2000-01-01","2023-12-31")
UNIV_MACRO <- function(date1,date2) {
  
  TMP <- retm %>%filter(STD_DT>date1 &STD_DT<date2)
 #TMP <- retm %>% filter(STD_DT>"2010-01-01")
EXPOSURE <<- lm(WORLD   ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame %>%
full_join(lm(KOSPI    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(WRBOND  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(KRBOND  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(WREPRA  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(WRINFRA ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(GSCI    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(PEF     ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(MSUS    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(EM      ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(USGOVT  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(USIG    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(USHY    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(HFRI    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(WTI     ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(GOLD    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
  full_join(lm(USGROWTH    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
  full_join(lm(USLONG    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
  full_join(lm(EMGOVTL    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
  full_join(lm(AL    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
  full_join(lm(US3M    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
  full_join(lm(USREIT    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
  full_join(lm(WRREIT    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(WORLD   ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(KOSPI    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(WRBOND  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(KRBOND  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(WREPRA  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(WRINFRA ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(GSCI    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(PEF    ~   GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(MSUS    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(EM      ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(USGOVT  ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(USIG    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(USHY    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(HFRI    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(WTI     ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(GOLD    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(USGROWTH    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>%  summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(USLONG    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(EMGOVTL    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(AL    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>%
full_join(lm(US3M    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>% 
  full_join(lm(USREIT    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>% 
  full_join(lm(WRREIT    ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary %>%.$coefficients%>%melt%>%as.data.frame, by=c("Var1","Var2"))%>% 
  
filter(Var2=="Estimate")



EXPOSURE[is.na(EXPOSURE)]<<-0
EXPOSURE <<-EXPOSURE[,-c(1,2)]%>%t%>%as.matrix
colnames(EXPOSURE)<<-c("UNEXPLAIN","성장","인플레이션","신용","실질금리","환율")
rownames(EXPOSURE)<<-c(
  "WORLD",
  "MSKR",
  "WRBOND",
  "KRBOND",
  "WREPRA",
  "WRINFRA",
  "GSCI",
  "PEF",
  'MSUS',
  'EM',
  'USGOVT',
  'USIG',
  'USHY',
  'HFRI',
  'WTI  ',
  'GOLD',
  "USGROWTH",
  "USLONG",
  "EMGOVTL",
  "AL",
  "US3M",
  "WORLD2",
  "MSKR2",
  "WRBOND2",
  "KRBOND2",
  "WREPRA2",
  "WRINFRA2",
  "GSCI2",
  "PEF2",
  'MSUS',
  'EM2',
  'USGOVT2',
  'USIG2',
  'USHY2',
  'HFRI2',
  'WTI2',
  'GOLD2',"USGROWTH2",
  "USLONG2",
  "EMGOVTL2",
  "AL2",
  "US3M2")


}

wei_reg<- EXPOSURE %>%melt%>%left_join(PORT_MACRO %>%filter(STD_DT==("2023-06-30")%>%as.Date)%>%dplyr::select(variable,wei),by=c("Var1"="variable"))%>%na.omit%>%mutate(exp=value*wei)%>%
  aggregate(exp~Var2,sum)
EXPOSURE2<-EXPOSURE[22:42,]
rownames(EXPOSURE2)<-c(
  "WORLD",
  "MSKR",
  "WRBOND",
  "KRBOND",
  "WREPRA",
  "WRINFRA",
  "GSCI",
  "PEF",
  'MSUS',
  'EM',
  'USGOVT',
  'USIG',
  'USHY',
  'HFRI',
  'WTI  ',
  'GOLD', "USGROWTH",
  "USLONG",
  "EMGOVTL",
  "AL",
  "US3M")

wei_step<- EXPOSURE2 %>%melt%>%left_join(PORT_MACRO %>%filter(STD_DT==("2023-06-30")%>%as.Date)%>%dplyr::select(variable,wei),by=c("Var1"="variable"))%>%na.omit%>%mutate(exp=value*wei)%>%
  aggregate(exp~Var2,sum)

EXP <- cbind(wei_reg,wei_step[,-1],EXPOSURE2[1,]%>%melt*0.6+0.4*EXPOSURE2[3,]%>%melt) 
RT_FACTOR2 <- retm%>%dplyr::select(STD_DT,GROWTH,INF,CREDIT,RINTEREST,FX)%>%filter(STD_DT>"2000-01-01")%>%cuml%>%left_join(MONTH%>%dplyr::select(STD_DT,USCPIYOY),by="STD_DT")

PORT_MACRO <- MPWEIGHT %>%mutate(STD_DT=STD_DT-days(1))%>% melt(id.vars = "STD_DT") %>%left_join(retm%>% melt(id.vars = "STD_DT"),by=c("STD_DT","variable"))
colnames(PORT_MACRO) <- c("STD_DT","variable","wei","value")
PORT_MACRO %>%dcast(STD_DT~variable,value.var = "wei")
EXPOSURE %>%melt%>%left_join(PORT_MACRO,by=c("Var1"="variable"))
PORT_MACRO %>% mutate(ret=value*wei)%>%dcast(STD_DT~variable,value.var = "ret")

write.xlsx(EXPOSURE ,  "c:/work/MACROPORT.xlsx", sheetName="EXPOSURE",append=F)
write.xlsx(ADJRSQ , "c:/work/MACROPORT.xlsx", sheetName="ADJRSQ",append=T)
write.xlsx(RET_MACRO ,  "c:/work/MACROPORT.xlsx", sheetName="RET_MACRO",append=T)
write.xlsx(tttmp , "c:/work/MACROPORT.xlsx", sheetName="tttmp",append=T)
write.xlsx(res1  , "c:/work/MACROPORT.xlsx", sheetName="res1",append=T)
write.xlsx(res2  ,"c:/work/MACROPORT.xlsx", sheetName="res2",append=T)
write.xlsx(res4  , "c:/work/MACROPORT.xlsx", sheetName="res4",append=T)
write.xlsx(res5  ,"c:/work/MACROPORT.xlsx", sheetName="res5",append=T)
write.xlsx(FACTOREXP_REG  , "c:/work/MACROPORT.xlsx", sheetName="WEI_REG",append=T)
write.xlsx(FACTOREXP_STEP  ,"c:/work/MACROPORT.xlsx", sheetName="WEI_STEP",append=T)
write.xlsx(EXP  , "c:/work/MACROPORT.xlsx", sheetName="EXP",append=T)
write.xlsx(RT_FACTOR  ,"c:/work/MACROPORT.xlsx", sheetName="RT_FACTOR",append=T)
write.xlsx(RT_FACTOR2  ,"c:/work/MACROPORT.xlsx", sheetName="RT_FACTOR2",append=T)
lm(WTI      ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary
lm(WTI     ~  GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% stepAIC(direction = "both",trace = FALSE)%>% summary 
  
res4 <- lm(KOSPI    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary%>%.$coefficients
res5 <- lm(KOSPI    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary%>%.$coefficients

ADJRSQ<- cbind(rbind(
  lm(KOSPI    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary%>%.$adj.r.squared, 
  lm(PEF  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary%>%.$adj.r.squared,
  lm(GSCI  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary%>%.$adj.r.squared,
  lm(HFRI  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary%>%.$adj.r.squared,
  lm(WTI ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary%>%.$adj.r.squared,
  lm(GOLD    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>% summary%>%.$adj.r.squared),
  rbind(lm(KOSPI    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary%>%.$adj.r.squared, 
        lm(PEF  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary%>%.$adj.r.squared,
        lm(GSCI  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary%>%.$adj.r.squared,
        lm(HFRI  ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary%>%.$adj.r.squared,
        lm(WTI ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary%>%.$adj.r.squared,
        lm(GOLD    ~ GROWTH+INF+CREDIT+RINTEREST+FX,data=TMP)%>%stepAIC(direction = "both",trace = FALSE)%>% summary%>%.$adj.r.squared))
