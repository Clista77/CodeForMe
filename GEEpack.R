library(geepack)
data(respiratory)
head(respiratory)
glm=glm(outcome~baseline+center+sex+treat+age+I(age^2),
        data=respiratory,family=binomial)
m.ex <- geeglm(outcome ~ baseline + center + sex + treat + age + I(age^2),
 data = respiratory, id = interaction(center, id),
 family = binomial, corstr = "exchangeable")
zcor <- genZcor(clusz = c(xtabs(~ id + center, data = respiratory)),
 waves = respiratory$visit, corstrv = 4)
m.toep <- geeglm(outcome ~ baseline + center + sex + treat + age + I(age^2),
 data = respiratory, id = interaction(center, id),
 family = binomial, corstr = "userdefined", zcor = zcor.toep)