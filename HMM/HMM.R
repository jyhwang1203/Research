
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("depmixS4","Rsolnp","Rdonlp2")

ipak(pkg)
# 패키지 불러오기
RAWDATA%>%filter(variable=="USA")%>%dcast(STD_DT~variable)%>%View
data1 <-  RAWDATA%>%filter(variable=="USA")%>%dcast(STD_DT~variable)%>%dplyr::select(USA)%>%na.omit%>%t%>%c
data2 <-  RAWDATA%>%filter(variable=="SP500")%>%filter(STD_DT>="1960-01-01")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%na.omit%>%round(4)%>%t%>%c
#data2 <-  retm %>% dplyr::select(INF)%>%na.omit%>%t%>%c
data3 <-  RAWDATA%>%filter(variable=="USCPIYOY")%>%dcast(STD_DT~variable)%>%dplyr::select(USCPIYOY)%>%na.omit%>%t%>%c

# 모델 정의
mod1 <- depmix(response = data1 ~ 1, family = gaussian(), nstates = 4, data = data.frame(data = data1))
mod2 <- depmix(response = data2 ~ 1, family = gaussian(), nstates = 2, data = data.frame(data = data2))
mod3 <- depmix(response = data3 ~ 1, family = gaussian(), nstates = 2, data = data.frame(data = data3))

# 모델 적합
fit.mod1 <-  depmixS4::fit(mod1)
fit.mod2 <-  depmixS4::fit(mod2)
fit.mod3 <-  depmixS4::fit(mod3)
summary(fit.mod1) #1 확장 2 후퇴
summary(fit.mod2) #1 확장 2 후퇴
summary(fit.mod3) #1 고   2 저


post_probs1 <- posterior(fit.mod1)
post_probs2 <- posterior(fit.mod2)
post_probs3 <- posterior(fit.mod3)
x1 <- RAWDATA%>%filter(variable=="USA")%>%dcast(STD_DT~variable)%>%dplyr::select(STD_DT)
x2 <- RAWDATA%>%filter(variable=="SP500")%>%filter(STD_DT>="1960-01-01")%>%dcast(STD_DT~variable)%>%trans_rt("month")%>%index%>%na.omit
x3 <- RAWDATA%>%filter(variable=="USCPIYOY")%>%dcast(STD_DT~variable)%>%dplyr::select(STD_DT)

data.frame(STD_DT=x1,data1,STATE1=post_probs1$state)%>%left_join(data.frame(STD_DT=x2,data2,STATE2=post_probs2$state),by="STD_DT")%>%left_join(data.frame(STD_DT=x3,data3,STATE3=(post_probs3$state)),by="STD_DT")%>%
  mutate(REGIME1=STATE1*(STATE3+1))%>%
  mutate(REGIME2=STATE2*(STATE3+1))%>%na.omit%>%View
  

(post_probs1$state)*(post_probs2$state+1)
(post_probs1$state)*(post_probs3$state+1)

ui <- navbarPage("Dashboard", theme = shinytheme("flatly"),
      tabPanel("HMM(USA)",
      dashboardPage(dashboardHeader(),
      dashboardSidebar(dateRangeInput('range',label = '',start = as.Date('2020-01-01') , end = as.Date('2023-12-31'))),
      dashboardBody(fluidRow(box(plotOutput("HMMOECD" ),width =4, solidHeader = TRUE),box(plotOutput("HMMSP500" ) , width=4, solidHeader = TRUE),box(plotOutput("HMMINF" ) , width =4, solidHeader = TRUE)),
                    fluidRow(box(plotOutput("HMMOECD2"),width =4, solidHeader = TRUE),box(plotOutput("HMMSP5002") , width=4, solidHeader = TRUE),box(plotOutput("HMMINF2") , width =4, solidHeader = TRUE)),
                    fluidRow(box(plotOutput("HMMOECD3"),width =4, solidHeader = TRUE),box(plotOutput("HMMSP5003") , width=4, solidHeader = TRUE),box(plotOutput("HMMINF3") , width =4, solidHeader = TRUE))
                    ))))
server <- function(input, output)
{   
  output$HMMOECD <- renderPlot({
    plot( x1,data1, type='l', main='Regime Detection', xlab='', ylab='Returns')
  })
  output$HMMSP500 <- renderPlot({
    plot( x2,data2, type='l', main='Regime Detection', xlab='', ylab='Returns')
  })
  output$HMMINF <- renderPlot({
    plot( x3,data3, type='l', main='Regime Detection', xlab='', ylab='Returns')
  })
  output$HMMOECD2 <- renderPlot({
    matplot(post_probs1[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')
  })
  output$HMMSP5002 <- renderPlot({
    matplot(post_probs2[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')
  })
  output$HMMINF2 <- renderPlot({
    matplot(post_probs3[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')
  })
  output$HMMOECD3 <- renderPlot({
    plot( x1,post_probs1$state, type='l', main='Regime Detection', xlab='', ylab='Returns')
  })
  output$HMMSP5003 <- renderPlot({
    plot( x2,post_probs2$state, type='l', main='Regime Detection', xlab='', ylab='Returns')
  })
  output$HMMINF3 <- renderPlot({
    plot( x3,post_probs3$state, type='l', main='Regime Detection', xlab='', ylab='Returns')
  })
  # output$HMMOECD2 <- renderPlot({
  #   CLI %>% dplyr::select(STD_DT,P11,P12)%>%cplot
  # })
  # output$HMMOECD3 <- renderPlot({
  #   CLI %>% dplyr::select(STD_DT,STATE1)%>%cplot
  # })
  # 
  # output$HMMINF <- renderPlot({
  #   plot( xxx,data2, type='l', main='Regime Detection', xlab='', ylab='Returns')
  # })
  # output$HMMINF2 <- renderPlot({matplot(post_probs2[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
  #   legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')
  # })
  # output$HMMINF3 <- renderPlot({
  #   plot( xxx,post_probs2$state, type='l', main='Regime Detection', xlab='', ylab='Returns')
  # })
  # 
  # output$SP500 <- renderPlot({
  #   #xx  %>% left_join(RAWDATA%>%filter(variable=="SP500")%>%dcast(STD_DT~variable), by="STD_DT") %>%cplot
  #   plot( TMP1$STD_DT,TMP1$SP500, type='l', main='Regime Detection', xlab='', ylab='Returns')
  # })
  # 
  # output$CPI <- renderPlot({
  #   plot( TMP2$STD_DT,TMP2$USCPIYOY, type='l', main='Regime Detection', xlab='', ylab='Returns')
  # })
  
}
shinyApp(ui, server)


CLI   <- data.frame(STD_DT=xx ,data=data1,STATE1=post_probs$state ,P11=post_probs$S1,P12=post_probs$S2)%>% left_join(RAWDATA%>%filter(variable=="SP500")%>%dcast(STD_DT~variable), by="STD_DT")%>%mutate(roll=rollmean(CLI$data, 12, fill = list(NA, NULL, NA)))
INF   <- data.frame(STD_DT=xxx ,data=data2,STATE2=post_probs2$state,P22=post_probs2$S1,P21=post_probs2$S2)%>% left_join(RAWDATA%>%filter(variable=="SP500")%>%dcast(STD_DT~variable), by="STD_DT")
CLI %>%dplyr::select(STD_DT,STATE1)%>%left_join(INF%>%dplyr::select(STD_DT,STATE2),by="STD_DT")%>% mutate(V5 = ifelse(STATE1 == 1 & STATE2 == 1, 1, ifelse(STATE1 == 1 & STATE2 == 2, 2, ifelse(STATE1 == 2 & STATE2 == 1, 3, 4))))


TMP1<- xx  %>% left_join(RAWDATA%>%filter(variable=="SP500")%>%dcast(STD_DT~variable), by="STD_DT") 
TMP2<- xxx %>% left_join(RAWDATA%>%filter(variable=="USCPIYOY")%>%dcast(STD_DT~variable), by="STD_DT")


CLI%>%View

INF$STATE

# final plot

plot(CLI$STD_DT,CLI$roll, type ="l", ylab = "beaver1 temperature",
     main = "Beaver Temperature Plot", xlab = "Time",
     col = "blue")
par(new = TRUE)
plot(CLI$STD_DT,CLI$STATE, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("beaver2 temperature", side = 4, line = 3)
legend("topleft", c("beaver1", "beaver2"),
       col = c("blue", "red"), lty = c(1, 2))


plot(INF$STD_DT,INF$data, type ="l", ylab = "beaver1 temperature",
     main = "Beaver Temperature Plot", xlab = "Time",
     col = "orange")
par(new = TRUE)
plot(INF$STD_DT,INF$STATE, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("beaver2 temperature", side = 4, line = 3)
legend("topleft", c("beaver1", "beaver2"),
       col = c("blue", "red"), lty = c(1, 2))

