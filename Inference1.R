library(infer)
rent=c(3850,3800,2350,3200,2150,3267,2495,2349,3950,1795,
       2145,2300,1775,2000,2175,2350,2550,4195,1470,2350)
hist(rent)
manhattan=data.frame(rent)
boot.rent.median=manhattan%>%specify(response=rent)%>%  
                             hypothesize(null="point",med=2500)%>% 
                             generate(reps=10000,type="bootstrap")%>%
                             calculate(stat="median")
boot.rent.median%>%ggplot(aes(x=stat))+geom_histogram(bins=50)
boot.rent.median%>%summarize(lower=quantile(stat,0.025),
                             upper=quantile(stat,0.975))
df=nrow(manhattan)-1
t.value=qt(0.975,df=df)
boot.se=sd(boot.rent.median$stat)
boot.rent.median%>%summarize(lower=median(rent) - t.value*boot.se,
                             upper=median(rent) + t.value*boot.se)
median(rent)#2350
boot.rent.median%>%filter(stat>=2350)%>%
                   summarize(p.value=n()/10000)

library(openintro)
data(ncbirths)
head(ncbirths)
hist(ncbirths$visits)
mean(ncbirths$visits,na.rm=T)
median(ncbirths$visits,na.rm=T)
ncbirths=ncbirths%>%filter(!is.na(visits))
boot.visits.mean=ncbirths%>%specify(response=visits)%>%
                            generate(reps=10000,type="bootstrap")%>%
                            calculate(stat="mean")
boot.visits.mean%>%summarize(lower=quantile(stat,0.05),
                             upper=quantile(stat,0.95))
boot.visits.sd=ncbirths%>%specify(response=visits)%>%
                          generate(reps=10000,type="bootstrap")%>%
                          calculate(stat="sd")
boot.visits.sd%>%summarize(lower=quantile(stat,0.05),
                           upper=quantile(stat,0.95))
boot.weight.mean=ncbirths%>%specify(response=weight)%>%
                            hypothesize(null="point",mu=7)%>%
                            generate(reps =10000,type="bootstrap")%>%
                            calculate(stat="mean")
mean(ncbirths$weight)#7.11
boot.weight.mean%>%filter(stat>=7.11)%>%
                   summarize(one.sided.p.value=n()/10000,
                             two.sided.p.value=2*one.sided.p.value)
