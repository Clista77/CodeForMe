library(Epi)
x=c(20,22,28,13,19,21,11,25,16,19)
y=c(rep(1,3),rep(0,7))
ROC(x,y)

library(epitools)
data(Titanic)
Titanic
Titanic=expand.table(Titanic)
head(Titanic)