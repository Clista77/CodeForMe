data(iris)
names(iris)=c('sl','sw','pl','pw','sp')
anova(aov(sl~sp,data=iris))
anova(aov(sw~sp,data=iris))
anova(aov(pl~sp,data=iris))
anova(aov(pw~sp,data=iris))

TukeyHSD(aov(sl~sp,data=iris))
plot(TukeyHSD(aov(sl~sp,data=iris)))

pairs(iris,col=iris$sp,pch=20)

model<-manova(as.matrix(iris[,1:4])~iris[,5])
summary(model)
summary(model,test="Wilks")$stats[1,2]

pca=princomp(iris[1:4])
summary(pca)
plot(pca)
loadings(pca)
biplot(pca)

iris.manova=manova(cbind(sl,sw,pl,pw) ~ sp,data=iris)
summary.aov(iris.manova)