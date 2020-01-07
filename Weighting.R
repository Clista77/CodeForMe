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
prop.diff=-apply(df2,2,diff)
prop=rbind(prop,prop.diff)

ATE.weight=n[3,1:5]/n$sum[3]
ATT.weight=n[1,1:5]/n$sum[1]

ATE=sum(prop[3,]*ATE.weight)
ATT=sum(prop[3,]*ATT.weight)

n
prop
ATE.weight
ATT.weight
ATE
ATT
sum(prop[3,])

#MMWS Margin Mean Weighting through Stratification
n_s
case.n_s
cont.n_s
case.prop=n$sum[1]/n$sum[3]
cont.prop=n$sum[2]/n$sum[3]
ATE.case.weight=(n[3,]*case.prop) / (n[1,])
ATE.cont.weight=(n[3,]*cont.prop) / (n[2,])
ATT.case.weight=(1)
ATT.cont.weight=(n[1,]*cont.prop) / (n[2,]*case.prop)

ATE.case=sum(prop[1,]*ATE.case.weight[1:5])
ATE.cont=
ATT.case=
ATT.cont=
