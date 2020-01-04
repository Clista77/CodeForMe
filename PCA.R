#Principle Component Analysis
library(GGally)
library(ggbiplot)
library(tidyverse)
data(iris)
ggpairs(iris,aes(color=Species))
iris=iris[1:4]
pca=prcomp(iris,scale=T)
summary(pca)
ggbiplot(pca)

library(MASS)
data(crabs)
pca=princomp(crabs[,4:8])
plot(pca)
X=as.matrix(crabs[,4:8]) %*% pc$loadings
library(mclust)
res12=Mclust(X[,1:2],G=4)
plot(res12)
res23=Mclust(X[,2:3],G=4)
plot(res23)

data(state)
dim(state.x77)
head(state.x77)
pca=princomp(state.x77,cor=T,scores=T) 
summary(pca)
plot(pca,type='l')
abline(h=0.5);grid()
pc.var=pca$sd^2
pc.pvar=pc.var/sum(pc.var)
plot(cumsum(pc.pvar),type='b')
abline(h=0.9);grid()
biplot(pca)

pca.scores=data.frame(pca$scores)
pca.scores%>%ggplot(aes(x=Comp.1,y=Comp.2,
                        label=rownames(pca.scores),
                        color=state.region))+ 
             geom_text(alpha=0.8,size=2)+
             ggtitle("PCA of states data colored by region")
library(factoextra)
fviz_pca_ind(pca)
fviz_pca_var(pca)
fviz_pca_biplot(pca)

state.dist=dist(state.x77)
state.cmds=cmdscale(state.dist) 
state.cmds.df=data.frame(state.cmds)
state.cmds.df%>%ggplot(aes(x=X1,y=X2,
                           label=rownames(state.cmds),
                           color=state.region))+
                geom_text(alpha=0.8,size=2)

library(rattle)
library(scatterplot3d)
data(wine)
head(wine)
wine.dist=dist(wine[,-1])
wine.cmds=cmdscale(wine.dist,k=3) 
wine.cmds.df=data.frame(wine.cmds)
scatterplot3d(wine.cmds.df,color=wine$Type,pch=20,type="h",lty.hplot=2)

