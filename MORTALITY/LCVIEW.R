#install.packages("shinydashboard")
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("shiny","shinythemes", "shinydashboard","DT")
ipak(pkg)
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/Mortalityfnv2.r")
source("c:/Users/ghkdw/OneDrive/문서/GitHub/HJPROJECT/FUNCTION/Quant UDF.r")
####
  
  ui <-     navbarPage("Dashboard", theme = shinytheme("flatly"),
               tabPanel("사망률모델",
    dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(selectInput("year", "년도", c(1970:2020))),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(shinydashboard::valueBox("Lee-Carter","Mortality",color="red", width = 12)
                  ),
      fluidRow(box(title = "ALPHA",DTOutput("lc2"), width = 4, solidHeader = TRUE),box(title = "BETA",DTOutput("lc3"), width = 4, solidHeader = TRUE),box(title = "KAPPA",DTOutput("lc4"), width = 4, solidHeader = TRUE)
                  ),
      fluidRow(box(plotOutput("lc"), width = 12, solidHeader = TRUE,background = "red")
                  ),
     # fluidRow(box(plotOutput("kt1"), width = 12, solidHeader = TRUE,background = "red")
      #            ), 
      fluidRow(box(plotOutput("mor2"), width = 12, solidHeader = TRUE,background = "red")
      ),
      fluidRow(box(plotOutput("mor3"), width = 12, solidHeader = TRUE,background = "red")
      ),
      # fluidRow(box(plotOutput("mor4"), width = 12, solidHeader = TRUE,background = "red")
      # ),
      fluidRow(box(plotOutput("mor5"), width = 12, solidHeader = TRUE,background = "red")
      ),
      fluidRow(box(plotOutput("mor6"), width = 12, solidHeader = TRUE,background = "red")
      )
                )
      
      )
    )
    )
  
  server <- function(input, output){
    
    output$lc2 <- renderDT({lcax%>%.[,-1]})
    output$lc3 <- renderDT({lcbx%>%.[,-1]})
    output$lc4 <- renderDT({lckt%>%.[,-1]})
    
    output$lc <- renderPlot({
      g1 <-  lcax %>%cplot+ggtitle("ALPHA") + theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g2 <-  lcbx %>%cplot+ggtitle("BETA")  + theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g3 <-  lckt %>%cplot+ggtitle("KAPPA") + theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      
      grid.arrange(g1,g2,g3,ncol = 3, nrow = 1)
    })
    
    output$kt1 <- renderPlot({
        g1 <-    FOR_TOTAL$res %>%as.data.frame()%>%dplyr::select(-LSTM) %>%na.omit%>%cplot + ggtitle("ARIMA예측치 VS 실제(전체)") + 
               theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g2 <-    FOR_MALE$res%>%data.frame%>%dplyr::select(-LSTM)%>%na.omit%>%cplot+ ggtitle("ARIMA예측치 VS 실제(남자)") + 
               theme(plot.title = element_text(size =  18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g3 <-    FOR_FEMALE$res%>%as.data.frame()%>%dplyr::select(-LSTM) %>%na.omit%>%cplot+ ggtitle("ARIMA예측치 VS 실제(여자자)") + 
               theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      grid.arrange(g1,g2,g3,ncol=3)
    })
    
    
    output$cusum <- renderPlot({
      data.frame(STD_DT=c(1970:2021),전체=cusum(data1),남자=cusum(data2),여자=cusum(data3))%>%cplot+ggtitle("Cusum 통계량") + 
        theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
    })
    
    output$kt2 <- renderPlot({
      
      g1 <-  FOR_TOTAL$res%>%data.frame %>%cplot+ggtitle("ARIMA VS LSTM VS 실제")+theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g2 <-  FOR_MALE$res %>%data.frame %>%cplot+ggtitle("ARIMA VS LSTM VS 실제")+theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g3 <-  FOR_FEMALE$res%>%data.frame%>%cplot+ggtitle("ARIMA VS LSTM VS 실제")+theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      
      grid.arrange(g1,g2,g3,ncol = 3)
    })
    
    output$biased1 <- renderDT({FOR_TOTAL$mse%>%round(3)  %>% data.frame})
    output$biased2 <- renderDT({FOR_MALE$mse%>%round(3)   %>% data.frame})
    output$biased3 <- renderDT({FOR_FEMALE$mse%>%round(3) %>% data.frame})
    output$biased4 <- renderDT({pfind(FOR_TOTAL$res2%>%.[-c(1:nnn),]) %>%round(3)  %>% data.frame})
    output$biased5 <- renderDT({pfind(FOR_MALE$res2%>%.[-c(1:nnn),])  %>%round(3)  %>% data.frame})
    output$biased6 <- renderDT({pfind(FOR_FEMALE$res2%>%.[-c(1:nnn),]) %>%round(3) %>% data.frame})
    
    
    
    output$mor <- renderPlot({
      
      g1 <-  FOR_TOTAL$res2 %>%data.frame%>%filter(STD_DT>input$year) %>%cplot+ggtitle("25세 사망률(전체)") +theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g2 <-  FOR_MALE$res2  %>%data.frame%>%filter(STD_DT>input$year) %>%cplot+ggtitle("25세 사망률(남자)") +theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g3 <-  FOR_FEMALE$res2%>%data.frame%>%filter(STD_DT>input$year) %>%cplot+ggtitle("25세 사망률(여자)") +theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      grid.arrange(g1,g2,g3,ncol = 3, nrow = 1)
    })
    
    output$mor2 <- renderPlot({
    
      g4 <-  FOR_TOTAL$res2 %>%data.frame%>%filter(STD_DT>input$year) %>%cplot+ggtitle("45세 사망률(전체)") +theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g5 <-  FOR_MALE$res2  %>%data.frame%>%filter(STD_DT>input$year) %>%cplot+ggtitle("45세 사망률(남자)") +theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g6 <-  FOR_FEMALE$res2%>%data.frame%>%filter(STD_DT>input$year) %>%cplot+ggtitle("45세 사망률(여자)") +theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      grid.arrange(g4,g5,g6,ncol = 3, nrow = 1)
    })
    
    output$mor3 <- renderPlot({
  
      g7 <-  FOR_TOTAL$res2 %>%data.frame%>%filter(STD_DT>input$year) %>%cplot+ggtitle("65세 사망률(전체)") +theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g8 <-  FOR_MALE$res2  %>%data.frame%>%filter(STD_DT>input$year) %>%cplot+ggtitle("65세 사망률(남자)") +theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      g9 <-  FOR_FEMALE$res2%>%data.frame%>%filter(STD_DT>input$year) %>%cplot+ggtitle("65세 사망률(여자)") +theme(plot.title = element_text(size = 18, face = "bold"),legend.title=element_text(size=17),legend.text=element_text(size=17))
      grid.arrange(g7,g8,g9,ncol = 3, nrow = 1)
    })
      
  output$mor4 <- renderPlot({

      
      grid.arrange(MORM25,MORF25,ncol = 2, nrow = 1)
    })
  output$mor5 <- renderPlot({
    
    grid.arrange(MORM45,MORF45,ncol = 2, nrow = 1)
  })
  output$mor6 <- renderPlot({
    
    grid.arrange(MORM65,MORF65,ncol = 2, nrow = 1)
  })
  # output$kt3  <- renderPlot({
  # 
  #   
  #   grid.arrange(g14,g15,ncol = 2, nrow = 1)
  # })
  # }
  # 
  }
  
  shinyApp(ui, server)



  # xlsx::write.xlsx(res  ,"c:/work/PAPER.xlsx", sheetName="FUNDAMENTAL",append=F) 
  # xlsx::write.xlsx(FOR_FEMALE$mse ,"c:/work/PAPER.xlsx", sheetName="BRATE",append=T) 
  # write.xlsx(FOR_MALE$mse   ,"c:/work/PAPER.xlsx", sheetName="CLI",append=T) 
  #  
  # FOR_TOTAL$mse
  # FOR_FEMALE$mse
  # FOR_MALE$mse
  # 
  # FOR_TOTAL
  # FOR_FEMALE$mse
  # FOR_MALE$mse