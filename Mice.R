library(MASS)
library(mice)
library(missForest)
library(naniar)
data(nhanes)
summary(nhanes)
vis_miss(nhanes)
mice=mice(nhanes,m=10,maxit=10,method='pmm')
with=with(mice,lm(chl~age+bmi))
pool=pool(with)
nhanes
complete(mice)
densityplot(mice)
