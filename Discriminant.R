library(MASS)
data(iris)
lda=lda(Species~.,data=iris)
predict(lda,iris)$class