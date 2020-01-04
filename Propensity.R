#Propensity Score
library(Matching)
data(lalonde)
head(lalonde)
glm=glm(treat~age+educ+black+hisp+married+nodegr+re74+re75+u74+u75,
        data=lalonde,family=binomial)
summary(glm)
ps=glm$fitted
lm=lm(re78~treat+ps,data=lalonde)
summary(lm)

#IPW
y=lalonde$re78
x=lalonde$treat
weight=(x/ps)+(1-x)/(1-ps)
ipw.lm=lm(y~x,data=lalonde,weights=weight)
summary(ipw.lm)
ipw1=sum((y/ps)[x==1])/sum(1/ps[x==1])
ipw0=sum((y/(1-ps))[x==0])/sum(1/(1-ps)[x==0])

#DR
library(tidyverse)
lalonde1=lalonde%>%filter(treat==1)
lalonde0=lalonde%>%filter(treat==0)
n1  <- nrow(lalonde1)
n0  <- nrow(lalonde0)
lm1 <- lm(re78~age+educ+black+hispan+married+nodegree+re74+re75,data=lalonde1)
lm0 <- lm(re78~age+educ+black+hispan+married+nodegree+re74+re75,data=lalonde0)
y1  <- lalonde1$re78
y0  <- lalonde0$re78
ps1 <- ps[lalonde$treat==1]
ps0 <- ps[lalonde$treat==0]
dr1 <- 1/n1 * sum(y1+((1-ps1)/ps1)*(y1-lm1$fitted))
dr0 <- 1/n0 * sum(y0/(1-ps0)+(1-1/(1-ps0))*lm0$fitted)

Y=lalonde$re78
Tr=lalonde$treat
X=glm$fitted
lm(Y~Tr)
match=Match(Y=Y,Tr=Tr,X=X)
summary(match)
MatchBalance(treat~age+educ+black+hisp+married+nodegr+re74+re75+u74+u75,
             match.out=match,nboots=100,data=lalonde)
treat1=lalonde$treat
treat2=rep(1,nrow(lalonde))-treat1
treat12=cbind(treat1,treat2)
weight1=treat1/glm$fitted
weight2=treat2/(1-glm$fitted)
weight12=weight1+weight2
ipw=lm(Y~treat12-1,weight=weight12)
summary(ipw)

library(twang)
data(lindner)
head(lindner)
with(lindner,table(abcix,lifepres))
glm=glm(abcix~stent+height+female+diabetic+acutemi+ejecfrac+ves1proc,
        data=lindner,family=binomial)
ps=glm$fitted
abcix=lindner$abcix
life=ifelse(lindner$lifepres==0,0,1)
ipw1=sum((abcix*life/ps)/sum(abcix/ps))
ipw0=sum((1-abcix)*life/(1-ps)/sum((1-abcix)/(1-ps)))
glm=glm(life~abcix+ps,data=lindner,family=binomial)
Y=lindner$lifepres
Tr=lindner$abcix
X=glm$fitted
lm(Y~Tr)
match=Match(Y=Y,Tr=Tr,X=X)
summary(match)
MatchBalance(abcix~stent+height+female+diabetic+acutemi+ejecfrac+ves1proc,
             match.out=match,nboots=1000,data=lindner)

library(PSAgraphics)
data(lindner)
attach(lindner)
glm=glm(abcix~stent+height+female+diabetic+acutemi+ejecfrac+ves1proc,
        data=lindner,family=binomial)
ps=glm$fitted
lindner.s5=cut(ps,quantile(ps,seq(0,1,1/5)),include.lowest=T,labels=F)
lindner.s10=cut(ps,quantile(ps,seq(0,1,1/10)),include.lowest=T,labels=F)
box.psa(ejecfrac,abcix,lindner.s5,xlab="Strata",ylab="Covariate:ejecfrac",
        legend.xy=c(2,110))
box.psa(ejecfrac,abcix,lindner.s10,xlab="Strata",ylab="Covariate:ejecfrac",
        legend.xy=c(4,110))
cat.psa(stent,abcix,lindner.s5,xlab="Strata",ylab="Proportion for stent",
        catnames=c("No Stent","Stent"),barnames=c("Standard PCI","Abciximab"))
cat.psa(stent,abcix,lindner.s10,xlab="Strata",ylab="Proportion for stent",
        catnames=c("No Stent","Stent"),barnames=c("Standard PCI","Abciximab"))
loess.psa(log(cardbill),abcix,ps,ylab="Response:log(cardbill)")