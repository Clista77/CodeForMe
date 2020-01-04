library(rmeta)
data(cochrane)
cochrane
a=meta.MH(n.trt,n.ctrl,ev.trt,ev.ctrl,names=name,data=cochrane)
b=meta.DSL(n.trt,n.ctrl,ev.trt,ev.ctrl,names=name,data=cochrane)
par(mfrow=c(1,2))
plot(a,main="meta.MH")
plot(b,main="meta.DSL")

library(metafor)
data(dat.bcg)
data=escalc(measure="RR",ai=tpos,bi=tneg,ci=cpos,di=cneg,data=dat.bcg)
res=rma(yi,vi,data=data)
forest(res)

library(netmeta)
data(Senn2013)
drugs=Senn2013
x=netmeta(TE,seTE,treat1,treat2,studlab,data=Senn2013,
          sm="MD",comb.fixed=TRUE,comb.random=TRUE)
d1=decomp.design(x)
d2=netmeta:::decomp.tau(x,tau=x$tau)
r2=netrank(x,small="good")
forest(x,pooled='fixed')
forest(x,pooled='random')
netgraph(x, thickness='se.fixed')
title('Fixed Effect Model')
netgraph(x, thickness='se.random')
title('Random Effect Model')
netheat(x,random=F)
mtext('Fixed Effect Model',1,1,cex=1.8)
netheat(x,random=T,tau=x$tau)
mtext('Random Effect Model',1,1,cex=1.8)
x$TE.random=x$TE.fixed
x$pval.random=x$pval.fixed
r1=netrank(x,small="good") 

install.packages('gemts')
library(gemtc)
network=mtc.network(data.re=drugs)
plot(network)
summary(network)