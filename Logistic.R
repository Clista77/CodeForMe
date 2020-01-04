library(dplyr)
library(nnet)
data(iris)
names(iris)=c('sl','sw','pl','pw','sp')
mlogit=multinom(sp~sl+sw+pl+pw,data=iris)
summary(mlogit)
iris=iris[1:100,]
iris=iris%>%mutate(sp=recode(sp,'setosa'=1,'versicolor'=0))
logit=glm(sp~sl+sw+pl+pw,data=iris)
summary(logit)

library(faraway)
data(babyfood)
babyfood%>%mutate(risk=disease/(disease+nondisease),
                  odds=disease/nondisease,logit=log(disease/nondisease))
glm=glm(cbind(disease,nondisease)~sex+food,family=binomial,data=babyfood)
anova(glm,test='Chi')
coef(glm) #a,b1,b2,b3
fitted(glm) #risk=p=exp(y)/1+exp(y)
predict(glm) #logit=y=log(p/(1-p))=a+b*x
fitted(glm)*(babyfood[1]+babyfood[2]) #p*(n1+n2)
fitted(glm)/(1-fitted(glm))==exp(predict(glm)) #odds

library(Stat2Data)
data(MedGPA)
head(MedGPA)
lm=lm(Acceptance~GPA,data=MedGPA)
a=lm$coef[1]
b=lm$coef[2]
GPA=3.5
logit=(a+b*GPA) #logit=log(p/(1-p))
p=exp(logit)/1+exp(logit) #p=exp(y)/1+exp(y)
odds=exp(logit) #odds=p/(1-p)

glm=glm(Acceptance~GPA,data=MedGPA,family=binomial)
MedGPA%>%ggplot(aes(x=GPA,y=Acceptance))+geom_point()+
geom_smooth(method="glm",se=F,method.args=list(family="binomial"))

gpa=MedGPA[c(1,5)]
gpa=gpa%>%mutate(p=fitted(glm),odds=p/(1-p),logit=predict(glm))
gpa%>%ggplot(aes(x=GPA,y=p))+geom_point()+geom_line()
gpa%>%ggplot(aes(x=GPA,y=odds))+geom_point()+geom_line()
gpa%>%ggplot(aes(x=GPA,y=logit))+geom_point()+geom_line()

new_data <- data.frame(GPA = 3.51)
augment(glm, newdata = new_data, type.predict = "response")
predict(glm,new_data)
