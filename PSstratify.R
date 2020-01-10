#Stratification with Propensity Score
library(tidyverse)
s1=c(5,32)
s2=c(9,27)
s3=c(6,30)
s4=c(23,13)
s5=c(28,8)
n=data.frame(s1,s2,s3,s4,s5)
sum=rowSums(n)
n=cbind(n,sum)
sum=colSums(n)
n=rbind(n,sum)

s1=c(0.40,0.16)
s2=c(0.55,0.07)
s3=c(0.17,0.27)
s4=c(0.39,0.25)
s5=c(0.46,0.25)
prop=data.frame(s1,s2,s3,s4,s5)
prop.diff=-apply(prop,2,diff)
prop=rbind(prop,prop.diff)

total.n_s = n[3,1:5]
total.n = n$sum[3]
case.n_s = n[1,1:5]
case.n = n$sum[1]
cont.n_s = n[2,1:5]
cont.n = n$sum[2]

ATE.weight = total.n_s / total.n
ATT.weight = case.n_s / case.n

ATE=sum(prop.diff * ATE.weight)
ATT=sum(prop.diff * ATT.weight)

#MMWS Margin Mean Weighting through Stratification
case.prop_s = prop[1,]
cont.prop_s = prop[2,]
case.n.rate = case.n / total.n
cont.n.rate = cont.n / total.n

ATE.case.weight=(total.n_s * case.prop) / case.n_s
ATE.cont.weight=(total.n_s * cont.prop) / cont.n_s
ATT.case.weight=(1)
ATT.cont.weight=(case.n_s * cont.prop) / (cont.n_s * case.prop)
prop
