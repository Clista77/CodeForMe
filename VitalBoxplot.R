library(readxl)
library(tidyverse)

demo0=read_excel('RespectData.xlsx',sheet='DEMO_0')
demo1=read_excel('RespectData.xlsx',sheet='DEMO_1')

sbp0=demo0%>%select(SUBJECT,SBP)%>%rename(time0=SBP)
dbp0=demo0%>%select(SUBJECT,DBP)%>%rename(time0=DBP)
pulse0=demo0%>%select(SUBJECT,PULSE)%>%rename(time0=PULSE)
sbp1=demo1%>%filter(str_detect(visit,'12'))%>%select(SUBJECT,SBP_1)%>%
             rename(time1=SBP_1)
sbp2=demo1%>%filter(str_detect(visit,'24'))%>%select(SUBJECT,SBP_1)%>%
             rename(time2=SBP_1)
sbp3=demo1%>%filter(str_detect(visit,'36'))%>%select(SUBJECT,SBP_1)%>%
             rename(time3=SBP_1)
dbp1=demo1%>%filter(str_detect(visit,'12'))%>%select(SUBJECT,DBP_1)%>%
             rename(time1=DBP_1)
dbp2=demo1%>%filter(str_detect(visit,'24'))%>%select(SUBJECT,DBP_1)%>%
             rename(time2=DBP_1)
dbp3=demo1%>%filter(str_detect(visit,'36'))%>%select(SUBJECT,DBP_1)%>%
             rename(time3=DBP_1)
pulse1=demo1%>%filter(str_detect(visit,'12'))%>%select(SUBJECT,PULSE_1)%>%
             rename(time1=PULSE_1)
pulse2=demo1%>%filter(str_detect(visit,'24'))%>%select(SUBJECT,PULSE_1)%>%
             rename(time2=PULSE_1)
pulse3=demo1%>%filter(str_detect(visit,'36'))%>%select(SUBJECT,PULSE_1)%>%
             rename(time3=PULSE_1)

sbp=list(sbp0,sbp1,sbp2,sbp3)%>%reduce(inner_join,by='SUBJECT')
dbp=list(dbp0,dbp1,dbp2,dbp3)%>%reduce(inner_join,by='SUBJECT')
pulse=list(pulse0,pulse1,pulse2,pulse3)%>%reduce(inner_join,by='SUBJECT')
nrow(sbp)==c(nrow(sbp0),nrow(sbp1),nrow(sbp2),nrow(sbp3))
nrow(dbp)==c(nrow(dbp0),nrow(dbp1),nrow(dbp2),nrow(dbp3))
nrow(pulse)==c(nrow(pulse0),nrow(pulse1),nrow(pulse2),nrow(pulse3))

sbp=sbp%>%gather(key=time,value=value,-SUBJECT)%>%mutate(vital='sbp')
dbp=dbp%>%gather(key=time,value=value,-SUBJECT)%>%mutate(vital='dbp')
pulse=pulse%>%gather(key=time,value=value,-SUBJECT)%>%mutate(vital='pulse')

vital=rbind(sbp,dbp,pulse)
vital%>%ggplot(aes(x=time,y=value))+
        geom_boxplot(aes(fill=time))+
        facet_wrap(~vital)
