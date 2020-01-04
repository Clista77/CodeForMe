library(boot)
data=c(100,21,212,123,78,121,45,12,145)
boot=c()
for(i in 1:10000){
  sample=sample(data,length(data),replace=T)
  boot[i]=mean(sample)
}
hist(data)
mean(data)
quantile(data,p=c(0.025,0.975))
hist(boot)
mean(boot)
quantile(boot,p=c(0.025,0.975))

library(simpleboot)
boot.mean=one.boot(data,mean,10000)
boot.mean=boot.mean$t
boot.ci(boot.mean,conf=0.95)

library(infer)
rent=c(3850,3800,2350,3200,2150,3267,2495,2349,3950,1795,
       2145,2300,1775,2000,2175,2350,2550,4195,1470,2350)
hist(rent)
manhattan=data.frame(rent)
boot.rent.median=manhattan%>%specify(response=rent)%>%  
                             generate(reps=10000,type="bootstrap")%>%
                             calculate(stat="median")
boot.rent.median%>%ggplot(aes(x=stat))+geom_histogram(bins=50)
boot.rent.median%>%summarize(lower=quantile(stat,0.025),
                             upper=quantile(stat,0.975))
median(rent)
df=nrow(manhattan)-1 
t.value=qt(0.975,df=df)
boot.se=sd(rent.boot$stat)
boot.rent.median%>%summarize(lower=median(rent) - t.value*boot.se,
                             upper=median(rent) + t.value*boot.se)

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