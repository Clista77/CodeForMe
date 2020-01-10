library(rstan)
library(tidyverse)
n=nrow(rats)
rx=rats%>%pull(rx)
time=rats%>%pull(time)
status.0TF=rats%>%pull(status)==0
status.1TF=rats%>%pull(status)==1
status.sum=rats%>%pull(status)%>%sum()

data(cars)
head(cars)
cars.model="
data{
int n;
real[n] x;
real[n] y;
}
parameters{
real alpha;
real beta;
real<lower=0> sigma;
}
model{
  a ~ normal(0,10);
  b ~ normal(0,10);
  sigma ~ cauchy(0,5);
  y ~ normal(alpha+beta*x,sigma);
}
"
n=nrow(cars)
x=cars$speed
y=cars$dist
cars.data=list(n=n,x=x,y=y)
cars.stan=stan(model_code=cars.model,data=cars.data,iter=1000,chain=4)
post=extract(cars.stan)

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
