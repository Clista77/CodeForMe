library(rpart)
library(rpart.plot)
data(iris)
rpart=rpart(Species~.,data=iris)
rpart
rpart.plot(rpart)

library(randomForest)
iris.rf=randomForest(Species~.,data=iris)
iris.rf
MDSplot(iris.rf,iris$Species)
importance(iris.rf)
varImpPlot(iris.rf)

set.seed(100)
rate=0.7
index=sample(nrow(iris),nrow(iris)*rate)
train=iris[index,]
test=iris[-index,]
cat("train= ",nrow(train)," test =",nrow(test))

train.rf=randomForest(Species~.,data=train)
prediction=predict(train.rf,test)
table(prediction,test$Species)

library(forestFloor)
x=iris[1:4]
y=iris[5]
xy.rf=randomForest(X,Y,keep.forest=TRUE,keep.inbag=T)
xy.ff=forestFloor(xy.rf,x)
show(xy.ff)