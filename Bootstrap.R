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