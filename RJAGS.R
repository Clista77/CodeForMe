library(rjags)
library(tidyverse)
prior.a=rbeta(n=1000,shape1=1,shape2=1)
prior.b=rbeta(n=1000,shape1=45,shape2=55)
prior.c=rbeta(n=1000,shape1=100,shape2=100)
prior.abc=data.frame(sample=c(prior.a,prior.b,prior.c),
                     prior=rep(c("A","B","C"),each=1000))
prior.abc%>%ggplot(aes(x=sample,fill=prior))+
            geom_density(alpha=0.5)


p.value=seq(from=0,to=1,length=1000)
poll.value=rbinom(n=1000,size=10,prob=p.value)    
likelihood=data.frame(p.value,poll.value)
likelihood%>%ggplot(aes(x=p.value,y=poll.value,group=poll.value))+
             geom_density_ridges()
likelihood%>%ggplot(aes(x=p.value,y=poll.value,group=poll.value,fill=poll.value==5))+ 
             geom_density_ridges()

model="
model{
x ~ dbin(p, n)
p ~ dbeta(a, b)
}"
jags.model=jags.model(textConnection(model), 
           data = list(a=45,b=55,x=6,n=10),
           inits = list(.RNG.name="base::Super-Duper",.RNG.seed=100))
simulation=coda.samples(model=jags.model,variable.names=c("p"),n.iter=1000)
plot(simulation,trace=F)

library(lme4)
data(sleepstudy)
sleepstudy=sleepstudy%>%filter(Days%in%c(0,3))%>%
                        spread(key=Days,value=Reaction)%>%
                        rename('Day0'='0','Day3'='3')%>%
                        mutate(diff=Day3-Day0)    
sleepstudy%>%summarize(mean(diff),sd(diff))
sleep.model="
model{
for(i in 1:length(Y)){
    Y[i] ~ dnorm(m,s^(-2))
    }
    m ~ dnorm(50, 25^(-2))
    s ~ dunif(0, 200)
}"
jags.model=jags.model(textConnection(sleep.model),
                      data = list(Y = sleepstudy$diff),
                      inits = list(.RNG.name="base::Wichmann-Hill",.RNG.seed=111))
sleep.sim=coda.samples(model=jags.model,variable.names=c("m","s"),n.iter=1000) 
summary(sleep.sim) 
plot(sleep.sim,trace=FALSE)
plot(sleep.sim,density=FALSE)
sleep.chain=data.frame(simulation[[1]],iter=1:1000)
head(sleep.chain)

library(openintro)
data(bdims)
head(bdims)
bdims%>%ggplot(aes(x=hgt,y=wgt))+ 
        geom_point()+geom_smooth(method='lm',se=F)
weight_model <- "model{
    for(i in 1:length(Y)){
      Y[i] ~ dnorm(m[i], s^(-2))
      m[i] <- a + b * X[i]
    }
    a ~ dnorm(0, 200^(-2))
    b ~ dnorm(1, 0.5^(-2))
    s ~ dunif(0, 20)
}"
weight_jags <- jags.model(
  textConnection(weight_model),
  data = list(Y = bdims$wgt, X = bdims$hgt),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)
weight_sim=coda.samples(model=weight_jags,variable.names=c("a", "b", "s"),n.iter=1000)
plot(weight_sim)