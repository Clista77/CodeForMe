library(readxl)
library(ggfortify)
library(tidyverse)
rnd=read_excel('RespectData.xlsx',sheet='RND')
demo=read_excel('RespectData.xlsx',sheet='DEMO')
enro=read_excel('RespectData.xlsx',sheet='ENRO')
exit=read_excel('RespectData.xlsx',sheet='EXIT')

dm=rnd%>%select(SUBJECT,DMYN)%>%rename(dm=DMYN)
gender=demo%>%select(SUBJECT,GENDER)%>%rename(gender=GENDER)
group=enro%>%select(SUBJECT,RNDOM)%>%rename(group=RNDOM)
age=demo%>%select(SUBJECT,AGE)%>%rename(age=AGE)%>%
           mutate(age=if_else(age>=70,'1','2'))
exit=exit%>%select(SUBJECT,STOPDT)%>%rename(stop=STOPDT)%>%
            mutate(stop=as.character(stop))
exit$stop[which(!is.na(exit$stop))]=1
exit$stop[which(is.na(exit$stop))]=2

respect=list(group,gender,age,dm,exit)%>%reduce(inner_join,by='SUBJECT')
nrow(respect)==c(nrow(group),nrow(gender),nrow(age),nrow(dm),nrow(exit))

respect=data.frame(map(respect,as.factor))
gg1=respect%>%group_by(group)%>%count(gender)%>%
              mutate(percent=round(n/sum(n),2)*100)%>%
              ggplot(aes(x=group,y=n,fill=gender))
gg2=respect%>%group_by(group)%>%count(age)%>%
              mutate(percent=round(n/sum(n),2)*100)%>%
              ggplot(aes(x=group,y=n,fill=age))
gg3=respect%>%group_by(group)%>%count(dm)%>%
              mutate(percent=round(n/sum(n),2)*100)%>%
              ggplot(aes(x=group,y=n,fill=dm))
gg4=respect%>%group_by(group)%>%count(stop)%>%
              mutate(percent=round(n/sum(n),2)*100)%>%
              ggplot(aes(x=group,y=n,fill=stop))
autoplot(list(gg1,gg2,gg3,gg4))+
              geom_bar(stat='identity',position='dodge')+
              geom_text(aes(label=paste0(percent,'%')),size=3,
                        position=position_dodge(width=0.9),vjust=1)

library(compareGroups)
cg=compareGroups(group~gender+age+dm+stop,data=respect)
createTable(cg)

library(moonBook)
mytable(group~gender+age+dm+stop,data=respect)