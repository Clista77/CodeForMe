library(RODBC)
library(moonBook)
library(tidyverse)
library(lubridate)
library(predict3d)
osteo=odbcConnectAccess2007("Osteoporosis.accdb")
sqlTables(osteo)$TABLE_NAME

bmd=sqlFetch(osteo,"BMD")
data1=sqlFetch(osteo,"T_TTLDATA")
data2=sqlFetch(osteo,"T_提供")
basic=sqlFetch(osteo,"患者基本情報")
oldlab=sqlFetch(osteo,"採血・採尿データ")
newlab=sqlFetch(osteo,"採血・採尿データ（新）")
drugday=sqlFetch(osteo,"薬剤投与歴")
drugname=sqlFetch(osteo,"薬剤名")

bmd1=bmd%>%drop_na(検査日)%>%rename(LBMD='L-BMD',HBMD='H-BMD')
basic1=basic%>%drop_na(DOB)%>%select(1,2,5,6,7)
drugday1=drugday%>%filter(薬剤==13)%>%filter(投与日数>=365)%>%
         group_by(ID)%>%arrange(ID,投与日数)%>%slice(n())%>%
         select(2,3,4,5,7)

full=list(bmd1,basic1,drugday1)%>%reduce(inner_join,by='ID')
full$LBMD=full$LBMD%>%str_sub(1,5)
full$HBMD=full$HBMD%>%str_sub(1,5)
full=full%>%mutate_at('ID',as.character)
full=full%>%mutate_at(c('LBMD','HBMD'),as.character)%>%
            mutate_at(c('LBMD','HBMD'),as.numeric)
full=full%>%mutate_at(c('Weight','Height'),as.character)%>%
            mutate_at(c('Weight','Height'),as.numeric)
full=full%>%mutate(AGE=interval(ymd(DOB),ymd(検査日))%/%years(1))
full=full%>%mutate(BMI=round(Weight/(Height/100)^2,1))
full$Gender=full$Gender%>%fct_recode(M='m',M='ｍ')

full0=full%>%filter(!検査日%within%interval(薬剤1開始日,薬剤1終了日))
full0=full0%>%filter(interval(検査日,薬剤1開始日)%/%years(1)==0)%>%
              group_by(ID)%>%arrange(ID,検査日)%>%slice(n())%>%
              mutate(year=0)
full2=full%>%filter(検査日%within%interval(薬剤1開始日,薬剤1終了日))
full2=full2%>%filter(interval(薬剤1開始日,検査日)%/%years(1)==2)%>%
              group_by(ID)%>%arrange(ID,検査日)%>%slice(1)%>%
              mutate(year=2)
full0=full0%>%filter(!is.na(LBMD)&!is.na(HBMD))
full2=full2%>%filter(!is.na(LBMD)&!is.na(HBMD))
full0=full0%>%filter_all(any_vars(ID%in%full2$ID))
full2=full2%>%filter_all(any_vars(ID%in%full0$ID))
full02=bind_rows(full0,full2)%>%
       group_by(ID)%>%arrange(ID,検査日)%>%
       mutate(LBMD.increase=(LBMD-first(LBMD))/first(LBMD),
              HBMD.increase=(HBMD-first(HBMD))/first(HBMD))

full0.na=full02%>%filter(year==0)%>%mutate(LBMD.yn=NA,HBMD.yn=NA)
full2.yn=full02%>%filter(year==2)%>%
                  mutate(LBMD.yn=if_else(LBMD.increase>=0.03,'y','n'),
                         HBMD.yn=if_else(HBMD.increase>=0.03,'y','n'))
full02.yn=bind_rows(full0.na,full2.yn)%>%arrange(ID,検査日)%>%
          fill(LBMD.yn,.direction='up')%>%fill(HBMD.yn,.direction='up')

full2.LBMD.y=full2.yn%>%filter(LBMD.yn=='y')
full2.LBMD.n=full2.yn%>%filter(LBMD.yn=='n')
full2.HBMD.y=full2.yn%>%filter(HBMD.yn=='y')
full2.HBMD.n=full2.yn%>%filter(HBMD.yn=='n')
full2.LBMD.y%>%ungroup()%>%
               summarize(LBMD.increase.mean=mean(LBMD.increase),
                         LBMD.increase.sd=sd(LBMD.increase))
full2.LBMD.n%>%ungroup()%>%
               summarize(LBMD.increase.mean=mean(LBMD.increase),
                         LBMD.increase.sd=sd(LBMD.increase))
full2.HBMD.y%>%ungroup()%>%
               summarize(HBMD.increase.mean=mean(HBMD.increase),
                         HBMD.increase.sd=sd(HBMD.increase))
full2.HBMD.n%>%ungroup()%>%
               summarize(HBMD.increase.mean=mean(HBMD.increase),
                         HBMD.increase.sd=sd(HBMD.increase))

dataset=full02.yn%>%select(2,4,5,7,8,9,14,15,16,17,18,19,20)
mytable(year~.,data=dataset,digit=2)

shapiro.test(full02$LBMD)
shapiro.test(full02$HBMD)
var.test(full0$LBMD,full2$LBMD)
var.test(full0$HBMD,full2$HBMD)
t.test(full0$LBMD,full2$LBMD,paired=T)
t.test(full0$HBMD,full2$HBMD,paired=T)
wilcox.test(full0$LBMD,full2$LBMD,paired=T)
wilcox.test(full0$HBMD,full2$HBMD,paired=T)

library(lme4)
lmerL=lmer(LBMD~year+(year|yn),data=dataset)
lmerH=lmer(HBMD~year+(year|yn),data=dataset)
fixef(lmerL); ranef(lmerL)
fixef(lmerH); ranef(lmerH)
gather=dataset%>%gather(BMD,value,c(LBMD,HBMD))
gather%>%ggplot(aes(x=year,y=value,color=yn))+
                geom_point()+facet_grid(yn~BMD)+
                geom_smooth(method='lm',se=F)

library(Epi)
full2.10=full2.yn%>%mutate(yn=if_else(yn=='y',1,0))
x=full2.10$LBMD.increase
y=full2.10$yn
ROC(x,y)

glm=glm(yn~AGE+BMI+Gender,data=full2.10)
summary(glm)
ggPredict(lm(LBMD.increase~AGE,data=full2.yn))
ggPredict(lm(LBMD.increase~BMI,data=full2.yn))
predict3d(glm,radius=1)

full.yn%>%filter(AGE>=75)
full.yn%>%filter(BMI>=25)
full.yn%>%filter(Gender=='M')
full.logit=full.yn%>%mutate(AGE=if_else(AGE>=75,1,0),
                            BMI=if_else(BMI>=25,1,0),
                            Gender=if_else(Gender=='M',1,0))
logit=glm(LBMD.increase~AGE+BMI+Gender,data=full.logit)
summary(logit)

sd(full01$LBMD,na.rm=T)
power.t.test(d=0.03,sig.level=0.05,power=0.8,sd=0.17)