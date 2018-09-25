Data File:
wine <- read.table('winequality-red.csv', sep = ';', header = TRUE)

Histograms:
library(reshape2)
library(ggplot2)
d <- melt(wine[,])
ggplot(d,aes(x = value)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()

BoxPlots:
boxplot(fixed.acidity,xlab="fixed.acidity")  (do the same for each variable)
The number of quality level
nrow(subset(redwine,quality==8))
[1] 18
> nrow(subset(redwine,quality==3))
[1] 10
> nrow(subset(redwine,quality==4))
[1] 53
nrow(subset(redwine,quality==7))
[1] 199
Correlations:
cor(wine,method="pearson")

Linear regression
set.seed(123)
> train=sample(nrow(redwine),nrow(redwine)/2)
>linear=glm(quality~.+fixed.acidity:citric.acid+fixed.acidity:density+fixed.acidity:pH,data=redwine,subset=train) 
> summary(linear)
mean((quality-predict(linear,redwine))[-train]^2)
Linear regression bestsubset
> set.seed(123)
> train=sample(c(TRUE,FALSE),nrow(redwine)/2,rep=TRUE)
> test=(!train)
> library(leaps)
>  regfit.best=regsubsets(quality~.,data=redwine[train,],nvmax=11)
> test.mat=model.matrix(quality~.,data=redwine[test,])
> val.errors=rep(NA,11)
> for(i in 1:11){coefi=coef(regfit.best,id=i)
+ pred=test.mat[,names(coefi)]%*%coefi
+ val.errors[i]=mean((quality[test]-pred)^2)}
> val.errors
> which.min(val.errors)
Linear regression with higher orders
set.seed(123)
> train=sample(c(TRUE,FALSE),nrow(redwine)/2,rep=TRUE)
> test=(!train)
> library(leaps)
> regfit.best=regsubsets(quality~.,data=redwine[train,],nvmax=11)
regfit.best=regsubsets(quality~.+I(volatile.acidity^2)+I(sulphates^2)+I(alcohol^2),data=redwine[train,],nvmax=11)
> test.mat=model.matrix(quality~.,data=redwine[test,])
test.mat=model.matrix(quality~.+I(volatile.acidity^2)+I(sulphates^2)+I(alcohol^2),data=redwine[test,])
> val.errors=rep(NA,11)
> attach(redwine)
> for(i in 1:11){coefi=coef(regfit.best,id=i)
+ pred=test.mat[,names(coefi)]%*%coefi
+ val.errors[i]=mean((quality[test]-pred)^2)} 
> val.errors
> which.min(val.errors)
> coef(regfit.best,7)
Tree and random forest
wine <- read.csv("winequality-red.csv", header = T, sep = ";", na.strings = "NA")
attach(wine)
quality.class <- ifelse(quality<=4, "low", ifelse(quality>=7, "high", "medium"))
wine <- data.frame(wine, quality.class)
set.seed(123)
train <- sample(1:nrow(wine), nrow(wine)/2)
wine.train <- wine[train,]
wine.test <- wine[-train,]

library(tree)
train.tree <- tree(quality.class ~.-quality, data = wine.train)
plot(train.tree)
text(train.tree,pretty = 0, cex=0.8)
test.tree <- predict(train.tree, wine.test, type = "class")
table(test.tree, wine.test$quality.class)
1-(31+1+625)/800  # =0.17875
## prune tree using CV
set.seed(123)
train.cv <- cv.tree(train.tree, FUN=prune.misclass)
train.cv   # terminal node = 5 is the best can be found in output

plot(train.cv$size, train.cv$dev, type="b")
train.prune.cv <- prune.misclass(train.tree, best=5)
plot(train.prune.cv)
text(train.prune.cv, pretty = 0, cex=0.8)

test.prune.cv <- predict(train.prune.cv, wine.test, type = "class")
table(test.prune.cv, wine.test$quality.class)
1-(36+626)/800  # =0.1725  Actually does not improve so much.

## random forests
library(randomForest)
set.seed(123)
train.rf <- randomForest(quality.class~.-quality, data=wine.train, mtry=3, importance=T)
train.rf
importance(train.rf)
varImpPlot(train.rf)
test.rf <- predict(train.rf, newdata = wine.test)
table(test.rf, wine.test$quality.class)
1-(52+635)/800 #=0.1412 got improved
KNN:
library(class)
wine <- read.table('/Users/dashburn/Documents/winequality-red.csv', sep = ';', header = TRUE)
wine$quality <- ifelse(wine$quality %in% c(3,4), 'LOW', ifelse(wine$quality %in% c(5,6), 
'MEDIUM', 'HIGH'))
set.seed(123)
standardized.X <- scale(wine[,-12])
train <- sample(1:nrow(wine), nrow(wine)/2)
train.X <- standardized.X[train,]
test.X <- standardized.X[-train,]
train.Y <- wine$quality[train]
test.Y <- wine$quality[-train]
error <- NULL

for (k in 1:20){
  knncv <- NULL
  knncv <- knn.cv(train=train.X, cl = train.Y, k = k, l = 0, prob =TRUE, use.all = TRUE)
  error[k] <- sum(knncv != train.Y) / length(train.Y)
}

plot(1:20, error, xlab = 'K', ylab = 'Error', type = 'l')
points(x = 1:20, y = error, col = 'red')
knnFullFit <- knn(train=train.X, test=test.X, cl = train.Y, k = which.min(error), l = 0, prob =TRUE, use.all = TRUE)
table(test.Y, knnFullFit)
mean(test.Y != knnFullFit)
LDA:
library(MASS)
set.seed(123)
lda.fit <- lda(quality ~., data = wine, subset = train)
lda.pred <- predict(lda.fit, wine[-train,])
lda.class <- lda.pred$class
table(wine$quality[-train], lda.class)
mean(lda.class != wine$quality[-train])

QDA:
library(MASS)
set.seed(123)
qda.fit <- qda(quality ~., data = wine, subset = train)
qda.pred <- predict(qda.fit, wine[-train,])
qda.class <- qda.pred$class
table(wine$quality[-train], qda.class)
mean(qda.class != wine$quality[-train])
