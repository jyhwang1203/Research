RT_MON   <- RAWDATA %>% filter(variable=="WORLD"|variable=="WRBOND"|variable=="MSKR"|variable=="KRBOND"|variable=="WRINFRA"|variable=="WRERPRA"|
            variable=="MSUS"|variable=="MSEU"|variable=="MSCN"|variable=="MSJP"|variable=="EMEXCN"|        
            variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|variable=="EMBOND"|
            variable=="GSCI"|variable=="WTI"|variable=="GOLD") %>%
            dcast(STD_DT~variable)%>%trans_rt("month")%>%dt_trans%>%tail(n=1)%>%exname
RT_YTD  <- RAWDATA %>% filter(variable=="WORLD"|variable=="WRBOND"|variable=="MSKR"|variable=="KRBOND"|variable=="WRINFRA"|variable=="WRERPRA"|
           variable=="MSUS"|variable=="MSEU"|variable=="MSCN"|variable=="MSJP"|variable=="EMEXCN"|        
           variable=="WRGOVT"|variable=="WRIG"|variable=="WRHY"|variable=="EMBOND"|
           variable=="GSCI"|variable=="WTI"|variable=="GOLD") %>%
           dcast(STD_DT~variable)%>%trans_rt("year")%>%dt_trans%>%tail(n=1)%>%exname

RT_YTD%>%graphbar("steelblue","주요자산(2월)")
RT_MON%>%graphbar("steelblue","주요자산(YTD)")

RT <- rbind(RT_MON,RT_YTD)
RT
write.xlsx(RT ,"c:/work/monthly.xlsx", sheetName="RT",append=T)

tmp   <- RAWDATA%>%filter(variable=="WORLD"|variable=="WRBOND")%>%dcast(STD_DT~variable)%>% trans_rt("month")%>%dt_trans
ttmp  <- RAWDATA%>%filter(variable=="WORLD"|variable=="WRBOND")%>%dcast(STD_DT~variable)%>% trans_rt("week")%>%dt_trans
#ROLLING CORRELATION 5YEAR
ROLLCOR <- data.table(STD_DT=tmp$STD_DT ,ROLLIMG3YEAR =roll_cor(tmp$WORLD, tmp$WRBOND  , width = 36))%>%na.omit%>%full_join(
           data.table(STD_DT=ttmp$STD_DT,ROLLIMG52WEEK =roll_cor(ttmp$WORLD, ttmp$WRBOND, width = 52))%>%na.omit,by="STD_DT")%>%arrange(STD_DT)
write.xlsx(ROLLCOR ,"c:/work/monthly.xlsx", sheetName="ROLLCOR",append=F)

#ROLLING RT
RAWDATA%>%filter(variable=="WORLD"|variable=="WRBOND")%>%dcast(STD_DT~variable)%>%na.omit%>%trans_rt("month")%>%dt_trans()%>%
        mutate(rollrt_world = rollmean(WORLD,   k=36, fill=NA, align='right'))%>%
        mutate(rollrt_wrbond = rollmean(WRBOND, k=36, fill=NA, align='right'))%>%na.omit%>%
        dplyr::select(STD_DT,rollrt_wrbond,rollrt_world)%>%cplot
  