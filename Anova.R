data(iris)
names(iris)=c('sl','sw','pl','pw','sp')
anova(aov(sl~sp,data=iris))
anova(aov(sw~sp,data=iris))
anova(aov(pl~sp,data=iris))
anova(aov(pw~sp,data=iris))

TukeyHSD(aov(sl~sp,data=iris))
plot(TukeyHSD(aov(sl~sp,data=iris)))