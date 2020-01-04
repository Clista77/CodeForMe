library(dplyr)
library(tidyr)
year1=read.csv('year1.csv')
year2=read.csv('year2.csv')
year3=read.csv('year3.csv')

bone1=year1%>%select(subjid,visitdat,starts_with('aeloc'))
bone2=year2%>%select(subjid,visitdat,starts_with('aeloc'))
bone3=year3%>%select(subjid,visitdat,starts_with('aeloc'))
bone123=rbind(bone1,bone2,bone3)
bone123=bone123%>%group_by(subjid)%>%
                  arrange(subjid,visitdat)%>%
                  mutate(aenum_bone=seq(n()))%>%
                  select(subjid,visitdat,aenum_bone,starts_with('aeloc'))