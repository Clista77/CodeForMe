#Propensity Score
library(MatchIt)
library(Hmisc)
data(lalonde)
head(lalonde)
glm=glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
        data=lalonde,family=binomial)
summary(glm)
ps=predict(glm,type='response')
lm=lm(re78~treat+ps,data=lalonde)
summary(lm)
hist=histbackback(split(ps,lalonde$treat),main='Propensity Score')
barplot(-hist$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(hist$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)

m.it=matchit(treat~age+educ+black+hispan+married+nodegree+re74+re75,
             data=lalonde,method="subclass",subclass=5)
summary(m.it)
m.data=match.data(m.it)
m.glm=glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
          data=m.data,family=binomial)
m.ps=predict(m.glm,type='response')
m.hist=histbackback(split(m.ps,m.data$treat),main="Propensity Score")
barplot(-m.hist$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(m.hist$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)

m.it.sum=summary(m.it, standardize = TRUE)
plot(m.it.sum, interactive = FALSE)

library(moonBook)
mytable(treat~.,lalonde)
mytable(treat~.,m.data)

#IPW1
y=lalonde$re78
x=lalonde$treat
weight=(x/ps)+(1-x)/(1-ps)
ipw.lm=lm(y~x,data=lalonde,weights=weight)
summary(ipw.lm)
ipw1=sum((y/ps)[x==1])/sum(1/ps[x==1])
ipw0=sum((y/(1-ps))[x==0])/sum(1/(1-ps)[x==0])
ipw1-ipw0

#IPW2
y  <- lalonde$re78
z  <- lalonde$treat
ipwe1 <- sum((z*y)/ps)/sum(z/ps)
ipwe0 <- sum(((1-z)*y)/(1-ps))/sum((1-z)/(1-ps))
ipw1-ipw0

#IPW Causal Effect
y=lalonde$re78
x1=lalonde$treat
x2=rep(1,nrow(lalonde))-x1
x12=cbind(x1,x2)
p1=x1/glm$fitted
p2=x2/(1-glm$fitted)
p12=p1+p2
ipw=lm(y~x12-1,weight=p12)
summary(ipw)

#DR1
gfomula = treat~age+educ+black+hisp+married+nodegr+re74+re75+u74+u75
fomula <- re78~age+educ+black+hisp+married+nodegr+re74+re75+u74+u75
library(dplyr)
library(Matching)
data(lalonde)
head(lalonde)
glm=glm(gfomula,
        data=lalonde,family=binomial)
ps=predict(glm,type='response')
lalonde1=lalonde%>%filter(treat==1)
lalonde0=lalonde%>%filter(treat==0)
n1  <- nrow(lalonde1)
n0  <- nrow(lalonde0)
lm1 <- lm(fomula,data=lalonde1)
lm0 <- lm(fomula,data=lalonde0)
y1  <- lalonde1$re78
y0  <- lalonde0$re78
ps1 <- ps[lalonde$treat==1]
ps0 <- ps[lalonde$treat==0]
dr1 <- 1/n1 * sum(y1+((1-ps1)/ps1)*(y1-lm1$fitted))
dr0 <- 1/n0 * sum(y0/(1-ps0)+(1-1/(1-ps0))*lm0$fitted)
dr1-dr0

#DR2
n      <- nrow(lalonde)
y      <- lalonde$re78
z      <- lalonde$treat
data1  <- lalonde %>% filter(treat==1)
data0  <- lalonde %>% filter(treat==0)
glm=glm(gfomula,
        data=lalonde,family=binomial)
ps=predict(glm,type='response')
model1 <- lm(fomula,data=data1)
model0 <- lm(fomula,data=data0)
fitted1 <- predict(model1, lalonde)
fitted0 <- predict(model0, lalonde)
dre1    <- (1/n)*sum(y+((z-ps)/ps)*(y-fitted1))
dre0    <- (1/n)*sum(((1-z)*y)/(1-ps)+(1-(1-z)/(1-ps))*fitted0)
dre1-dre0

#DR3
n1  <- nrow(lalonde1)
n0  <- nrow(lalonde0)
z1=1
z0=0
y1  <- lalonde1$re78
y0  <- lalonde0$re78
ps1 <- ps[lalonde$treat==1]
ps0 <- ps[lalonde$treat==0]
dr11 = 1/n1 * sum((z1*y1)/(ps1)+(1-z1/ps1)*lm1$fitted)
dr00 = 1/n0 * sum((z0*y0)/(ps0)+(1-z0/ps0)*lm0$fitted)
dr11-dr00
predict1 <- predict(lm1, lalonde)
predict0 <- predict(lm0, lalonde)
#or maybe this?
or1  <- (1/n)*sum(    (z*y)/  (ps) + (1-  (z)/  (ps))*predict1)
or0  <- (1/n)*sum(((1-z)*y)/(1-ps) + (1-(1-z)/(1-ps))*predict0)
or1-or0

library(Epi)
ROC(form=treat~age+educ+black+hispan+married+nodegree+re74+re75,
    data=lalonde)
ROC(form=treat~age+educ+black+hispan+married+nodegree+re74+re75,
    data=m.data)

library(Matching)
glm=glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
        data=lalonde,family=binomial)
Y=lalonde$re78
Tr=lalonde$treat
X=glm$fitted
lm(Y~Tr)
match=Match(Y=Y,Tr=Tr,X=X)
summary(match)
MatchBalance(treat~age+educ+black+hispan+married+nodegree+re74+re75,
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
glm=glm(abcix~stent+height+female+diabetic+acutemi+ejecfrac+ves1proc,
        data=lindner,family=binomial)
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