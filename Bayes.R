x=seq(0,1,length=100)
plot(x,dbinom(x=8,prob=x,size=10))
lines(x,dnorm(x=x,mean=0.5,sd=0.1)/15)
like=dbinom(x=8,prob=x,size=10)
prior=dnorm(x=x,mean=0.5,sd=0.1)
lines(x,like*prior)

n=10000
ads=100
prob=runif(n=n,min=0,max=1)
click=rbinom(n=n,size=ads,prob=prob)
hist(prob)
hist(click)
prior=data.frame(prob,click)
plot(prior)
posterior=prior[prior$click==10,]
par(mfrow=c(2,1))
hist(prior$prob)
hist(posterior$prob)
prior=posterior
n=nrow(prior)
ads=100
prior$click=rbinom(n,size=ads,prob=prior$prob)
hist(prior$prob)
hist(prior$click)
sum(prior$click>=5)/length(prior$click)



