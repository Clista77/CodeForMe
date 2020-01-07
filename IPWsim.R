#N=1000
#N(L=0)=730
#N(L=1)=270
#N(A=1|L=0)=0.315*730
#N(A=0|L=0)=730-0.315*730
#N(A=1|L=1)=(0.315+0.352)*270
#N(A=0|L=1)=270-(0.315+0.352)*270
#P(Y=1|L=0,A=0)=invlogit(-2.5)
#P(Y=0|L=1,A=0)=invlogit(-2.5+1.75)
#P(Y=1|L=0,A=1)=invlogit(-1.5)
#P(Y=0|L=1,A=1)=invlogit(-1.5+1.75)
#IPW(Y=1|L=0,A=0)=1/(N(A=0|L=0)/N(L=0))=1/((730-0.315*730)/730)
#IPW(Y=1|L=0,A=1)=1/(N(A=1|L=0)/N(L=0))=1/(0.315*730/730)
#IPW(Y=1|L=1,A=0)=1/(N(A=0|L=1)/N(L=0))=1/((270-(0.315+0.352)*270)/270)
#IPW(Y=1|L=1,A=1)=1/(N(A=1|L=1)/N(L=0))=1/((0.315+0.352)*270/270)

#Functions
odds=function(p){return(p/(1-p))}
logit=function(p){return(log(p/(1-p)))}
invlogit=function(p){return(exp(p)/(1+exp(p)))}

#P(L=0)=0.73
#P(L=1)=0.27
L=rbinom(1000,1,0.27)

#P(A=0)=0.315
#P(A=1)=0.315+0.352
A=rbinom(1000,1,0.315+0.352*L)

#P(Y=1|L=0,A=0)=invlogit(-2.5)
#P(Y=0|L=1,A=0)=invlogit(-2.5+1.75)
p0=plogis(-2.5+1.75*L)#invlogit
Y0=rbinom(1000,1,prob=p0)

#P(Y=1|L=0,A=1)=invlogit(-1.5)
#P(Y=0|L=1,A=1)=invlogit(-1.5+1.75)
p1=plogis(-1.5+1.75*L)#invlogit
Y1=rbinom(1000,1,prob=p1)

#P(Y=1)
Y=Y0+A*(Y1-Y0)
df=data.frame(L,A,Y0,Y1,Y)

#Marginal log-odds-ratio(LOR)
log(odds(mean(Y1))/odds(mean(Y0)))

glm(Y~A+L,data=df,family="binomial")
exposure=glm(A~L,data=df,family="binomial")
pA=predict(exposure,type="response")
pA.actual=(A*pA)+((1-A)*(1-pA))
IPW=1/pA.actual
df=data.frame(df,pA,pA_actual,IPW)
glm(Y~A,data=df,family="binomial",weights=IPW)


