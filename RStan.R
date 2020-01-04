library(rstan)
library(tidyverse)
n=nrow(rats)
rx=rats%>%pull(rx)
time=rats%>%pull(time)
status.0TF=rats%>%pull(status)==0
status.1TF=rats%>%pull(status)==1
status.sum=rats%>%pull(status)%>%sum()

library(openintro)
data(bdims)
head(bdims)
bdims.model="
data{
int n;
real x[n];
real y[n];
}
parameters{
real a;
real b;
real<lower=0> sigma;
}
model{
  for(i in 1:n){
  y[i] ~ normal(a+b*x[i],sigma);
  }
}
"
n=nrow(bdims) ;x=bdims$wgt; y=bdims$hgt
bdims.data=list(n=n,x=x,y=y)
bdims.stan=stan(model_code=bdims.model,data=bdims.data,iter=1000,chain=4)
post=extract(bdims.stan)

library(ggmcmc)
ggmcmc(ggs(stan))

library(lme4)
library(rstanarm)
library(bayesplot)
data(sleepstudy)
sleep=sleepstudy
lmer=lmer(Reaction~Days+(Days|Subject),data=sleep)
fixef(lmer)
ranef(lmer)
stanlmer=stan_lmer(Reaction~Days+(Days|Subject),data=sleep)
fixef(stanlmer)
ranef(stanlmer)
sleep%>%ggplot(aes(x=Days,y=Reaction,color=Subject))+
        geom_point()+geom_smooth(method='lm',se=F)
