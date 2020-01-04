data(airquality)
mlm=lm(Ozone~Solar.R+Wind+Temp,data=airquality)
summary(mlm)
library(car)
vif(mlm)
mlm2=step(mlm)

library(MCMCpack)
MCreg=MCMCregress(Ozone~Solar.R+Wind+Temp,data=airquality)
summary(MCreg)
plot(MCreg)

m=0
sd=1
sample=rnorm(1000,m,sd)
mean(sample)
cummean <- function(x)
    cumsum(x)/1:length(x)
plot(cummean(sample),type='l',las=1)
abline(h=0,col="red")
