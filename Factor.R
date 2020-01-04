library(psych)
data(bfi)
bfi=bfi[1:25]
head(bfi)
scree(bfi,factor=F)
fa=fa(bfi,nfactors=6)
fa$loadings
fa.diagram(fa)
head(fa$scores)
summary(fa$scores)
plot(density(fa$scores,na.rm=T),main="Factor Scores")
describe(bfi)
error.dots(bfi)
error.bars(bfi)

n=nrow(bfi)
p=0.5
seq=seq(1,n)
seq.efa=sample(seq,n*p)
seq.cfa=seq[!(seq%in%seq.efa)]
bfi.efa=bfi[seq.efa,]
bfi.cfa=bfi[seq.cfa,]

#Basic Option
cor=cor(bfi,use='complete.obs')
eigen=eigen(cor)$values
plot(eigen,type='b')
factanal=factanal(na.omit(bfi),factors=6)
factanal


