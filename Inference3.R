library(NHANES)
names(NHANES)
NHANES%>%ggplot(aes(x=Gender,fill=HomeOwn))+
         geom_bar(position="fill")+
         ylab("Relative Frequency")
NHANES%>%ggplot(aes(x=SleepHrsNight,color=SleepTrouble))+
         geom_density(adjust=3)+
         facet_wrap(~HealthGen)
home=NHANES%>%select(Gender,HomeOwn)%>%
              filter(HomeOwn%in%c("Own","Rent"))
home%>%group_by(Gender)%>%
       summarize(own.prop=mean(HomeOwn=="Own"))%>%
       summarize(own.prop.diff=diff(own.prop))

n=100
home.perm=home%>%specify(HomeOwn~Gender,success="Own")%>%
                 hypothesize(null="independence")%>%
                 generate(reps=n,type="permute")%>% 
                 calculate(stat="diff in props",order=c("male","female"))
home.perm
home.perm%>%ggplot(aes(x = stat))+geom_dotplot()
home.perm%>%ggplot(aes(x = stat))+geom_density()