# 3.02 in class work
library(ISLR)
set.seed(1)
train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
# Cubic regression line
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
set.seed(2)
train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# kknn
library(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
              prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
                  kernel = "triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol[(iris.valid$Species != fit)+1])
# 3.5 lab
set.seed(12345)
help(par)
par(mar = rep(0.2,4))
data_Matrix <-matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
par(mar=rep(0.2,4))

# hierarchical clustering
heatmap(data_Matrix)
help(rbinom)
set.seed(678910)
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}
par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])
par(mar= rep(0.2, 4))
heatmap(data_Matrix)

data(swiss)
sclass <- kmeans(swiss[2:6], 3) 
table(sclass$cluster, swiss[,1])    
# 
library(e1071)
m <- naiveBayes(swiss[2:6], swiss[,1])    
table(predict(m, iswiss[2:6], swiss[,1])
require(rpart)
data("Titanic")
# dicision tree
Titanic_rpart <- rpart(Survived ~., data = Titanic)
plot(Titanic_rpart)
text(Titanic_rpart)
printcp(Titanic_rpart)
# random forest
require(randomForest)
fitKF <- randomForest(Survived ~.,   data=Titanic)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor
varImpPlot(fitKF)
plot(fitKF)
getTree(fitKF,1, labelVar=TRUE)
#  ctree
require(party)
tree_Titanic<-ctree(Survived ~ ., data=Titanic)
plot(tree_Titanic)
cforest(Survived ~ ., data=Titanic, controls=cforest_control(mtry=2, mincriterion=0))
treeFert<-ctree(Survived ~ ., data = Titanic)
# naive bayers model
require(mlbench)
data(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
predict(m, as.data.frame(Titanic)[,1:3])

## Example with metric predictors:
data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris[,-5]), iris[,5])
