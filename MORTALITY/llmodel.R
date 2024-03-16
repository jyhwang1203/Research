# llmodel
# data2 <- BASEKR
# data <- BASEKRM
# dataf <- BASEKRF$Dxt/BASEKRF$Ext


llmodel <- function(data,data2){
  # data  <-  BASEKRF
  # data2 <-  BASEKR
  data <-  data$Dxt/data$Ext
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
  
  
  res$a <- a
  res$b <- b
  res$k <- k
  res$B <- LCfitt$bx
  res$K <- LCfitt$kt
  
  
  return(res)
}


# 
# 
# 
# a.svd<-svd(zm)
# u<-a.svd$u
# v<-a.svd$v
# d<-a.svd$d
# 
# 
# l=max(d)
# for(x in 1:nrow(data)){
#   
#   for(t in 1:ncol(data))	{
#     e[x,t]=l*u[x,1]*v[t,1]
#   }
# }
# 
# for(t in 1:ncol(data)){
#   for(x in 1:nrow(data))
#   {
#     km[1,t]=e[x,t]+km[1,t]
#   }
# }
# 
# for(x in 1:nrow(data)){
#   bm[x,1]=e[x,2]/km[1] 
# }
# 
# llm <- am + (LCfitt$bx%*% LCfitt$kt)  + bm%*%km
# lcm <- LCfitm$ax +(LCfitm$bx%*% LCfitm$kt)
# rlm <- BASEKRM$Dxt/BASEKRM$Ext
# 
# 
# STD_DT <- c(1970:2021)
# tmp1<- cbind(exp(llf[45,]),exp(lcf[45,]),(rlf[45,]))
# colnames(tmp1)[1:3] <- c("ll","lc","read")
# 
# g1 <- tmp1%>%melt(id.vars="STD_DT")%>%ggplot(aes(x=Var1, y=value, col = Var2)) +             
#   geom_line()
# 
# 
# STD_DT <- c(1970:2021)
# tmp2 <- cbind(exp(llm[45,]),exp(lcm[45,]),(rlm[45,]))
# colnames(tmp2)[1:3] <- c("ll","lc","read")
# 
# g2 <-tmp2%>%melt(id.vars="STD_DT")%>%ggplot(aes(x=Var1, y=value, col = Var2)) +             
#   geom_line()
# 
# grid.arrange(g1,g2 ,
#              ncol = 2, nrow = 1)
# f=exp(a+b%*%k)
# 
# LCfit$kt
# k
# f=exp(f)

# real=0
# 
# slope=0
# ori=k[1,1]
# for(n in 1: 50)
#   dea <- BASEKR$Dxt
# sur <- BASEKR$Ext
# 
# for(n in 1:100){
# for(t in 1:52){
# slope=0
# pre[1,t]=0
# for(x in 1:100)
# {
# 
# pre[1,t]=pre[1,t]-dea[x,t]+sur[x,t]*exp(a[x,t]+b[x,1]*k[1,t])
# slope=slope+sur[x,t]*b[x,1]*exp(a[x,t]+b[x,1]*k[1,t])
# 
# }
# 
# k[1,t]=k[1,t]-pre[1,t]/slope
# 
# }
# 
# }



# plot(f[20,],xlab='20')
# 
