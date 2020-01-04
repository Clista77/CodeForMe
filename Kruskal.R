kruskal.test(gender~group,data=respect)
kruskal.test(age~group,data=respect)
kruskal.test(dm~group,data=respect)
kruskal.test(stop~group,data=respect)
manova(cbind(gender,age,dm,stop)~group,data=respect)

head(cl)
  Age LowOrNot LastWeight Smoke HTN Pain
1  16        1        130     0   0    0
2  16        0        110     0   0    0
3  16        0        112     0   0    0
4  16        0        135     1   0    0
5  16        0        135     1   0    0
6  16        0        170     0   0    0
clogit=clogit(LowOrNot~LastWeight+Smoke+HTN+Pain+strata(Age),data=cl)

full1=full1%>%mutate(y=1)
full2=full2%>%mutate(y=0)
full12=rbind(full1,full2)
clogit=clogit(y~AGE+BMI+strata(Gender),data=full12)

L=read.csv('Logit.csv')
logit=glm(Result~YesNo+Study,family=binomial,data=L)
summary(logit)

full12$Gender=full12$Gender%>%fct_recode('1'='M','2'='F')
full12=full12%>%mutate_at('Gender',as.numeric)
logit=glm(y~AGE+BMI+Gender,data=full12)
summary(logit)
